module Data.Voxel.Viewer(
    runViewer
  ) where

import Control.Lens ((^.))
import Control.Monad
import Data.Bits
import Data.Int
import Data.MagicaVoxel.Types (RGBA(..))
import Data.Maybe
import Data.Proxy
import Data.Voxel.GPipe.Mesh
import Data.Voxel.Grid.Unbox.Polygon
import Data.Voxel.MagicaVoxel (convertMagica)
import Data.Word
import Graphics.GPipe
import Data.STRef

import qualified Data.MagicaVoxel as MV 
import qualified Data.Vector.Storable as V
import qualified Data.Voxel.Grid.Unbox as G
import qualified Data.Voxel.Grid.Unbox.Mutable as GM
import qualified Data.Voxel.Mesh as M
import qualified Graphics.GPipe.Context.GLFW as GLFW
import qualified Math.Noise as N 

xorshf96Init :: V3 Int 
xorshf96Init = V3 123456789 362436069 521288629

-- | Fast Marsaglia's xorshf generator. Period 2^96-1. 
-- Use z component as resulted random value.
xorshf96 :: V3 Int -> V3 Int
xorshf96 (V3 x0 y0 z0) = (V3 x' y' z')
  where 
    x1 = x0 `xor` (x0 `unsafeShiftL` 16)
    x2 = x1 `xor` (x1 `unsafeShiftR` 5)
    x3 = x2 `xor` (x2 `unsafeShiftL` 1)

    x' = y0
    y' = z0
    z' = x3 `xor` x' `xor` y' 


generateMap :: G.VoxelGrid (V3 Float)
generateMap = G.create $ do 
  let n = 128
  let maxz = 32
  let scale = 0.005
  m <- GM.new (V3 n n maxz)
  r <- newSTRef xorshf96Init
  forM_ [(x, y)| x <- [0 .. n-1], y <- [0 .. n-1]] $ \(x, y) -> do
    let zv = fromMaybe 0 $ N.getValue N.perlin (scale * fromIntegral x, scale * fromIntegral y, 1.0) 
    let mz = fromIntegral maxz
    let z = round $ max 0 $ min (mz - 1) $ mz * 0.5 + zv * mz
    forM_ [0 .. z] $ \i -> do  
      V3 rx _ rz <- readSTRef r 
      modifySTRef' r xorshf96 
      let dv = if rx `mod` 2 == 0 then 0   
            else 0.05 * fromIntegral rz / (fromIntegral (maxBound :: Int))
      let col = V3 (1.0 + dv) (158 / 255 + dv) (68 / 255 + dv)
      GM.write m (V3 x y i) col
  pure m

runViewer :: IO ()
runViewer = do
  runContextT GLFW.defaultHandleConfig $ do

    let wcfg = (GLFW.defaultWindowConfig "voxel viewer") {
            GLFW.configWidth = 800
          , GLFW.configHeight = 600
          }
    win <- newWindow (WindowFormatColorDepth RGBA8 Depth16) wcfg
    voxModel <- either (fail . ("Vox loading: " ++)) pure =<< MV.parseFile "../MagicaVoxel-vox/test/house.vox"
    rawModel <- either (fail . ("Vox convert: " ++)) pure $ convertMagica voxModel
    -- Create vertex data buffers
    let model :: G.VoxelGrid (V3 Float)
        model = generateMap -- G.map word32Color rawModel
    buffers <- meshBuffers $ triangulate TriangulateTriangles model
    -- Make a Render action that returns a PrimitiveArray for the cube
    let makePrimitives = meshBufferArray (Proxy :: Proxy (V3 Float)) TriangleList buffers

    -- Create a buffer for the uniform values
    uniform :: Buffer os (Uniform (V4 (B4 Float), V3 (B3 Float))) <- newBuffer 1

    -- Create the shader
    shader <- compileShader $ do
      sides <- toPrimitiveStream primitives
      (modelViewProj, normMat) <- getUniform (const (uniform, 0))
      let projectedSides = proj modelViewProj normMat <$> sides

      fragNormalsUV <- rasterize rasterOptions projectedSides
      let litFrags = uncurry light <$> fragNormalsUV
          litFragsWithDepth = withRasterizedInfo
              (\a x -> (a, rasterizedFragCoord x ^. _z)) litFrags
          colorOption = ContextColorOption NoBlending (pure True)
          depthOption = DepthOption Less True

      drawWindowColorDepth (const (win, colorOption, depthOption)) litFragsWithDepth

    -- Run the loop
    loop win shader makePrimitives uniform 0

loop :: Window os RGBAFloat Depth
  -> (ShaderEnvironment -> Render os ())
  -> Render
       os (PrimitiveArray Triangles (MeshArray (ArrayOf (V3 Float))))
  -> Buffer os (Uniform (V4 (B4 Float), V3 (B3 Float)))
  -> Float
  -> ContextT GLFW.Handle os IO ()
loop win shader makePrimitives uniform ang = do
  -- Write this frames uniform value
  size@(V2 w h) <- getFrameBufferSize win
  let modelRot = fromQuaternion (axisAngle (V3 0 0 1) ang)
      modelMat = mkTransformationMat modelRot (pure 0) !*! mkTransformationMat identity (-V3 0.5 0.5 0)
      projMat = perspective (pi/9) (fromIntegral w / fromIntegral h) 1 100
      viewMat = mkTransformation (axisAngle (V3 1 0 0) (-pi/3)) (- V3 0 0 5)
      viewProjMat = projMat !*! viewMat !*! modelMat
      normMat = modelRot
  writeBuffer uniform 0 [(viewProjMat, normMat)]

  -- Render the frame and present the results
  render $ do
    clearWindowColor win 0 -- Black
    clearWindowDepth win 1 -- Far plane
    prims <- makePrimitives
    shader $ ShaderEnvironment prims (FrontAndBack, ViewPort 0 size, DepthRange 0 1)
  swapWindowBuffers win

  closeRequested <- GLFW.windowShouldClose win
  unless (fromMaybe False closeRequested) $
    loop win shader makePrimitives uniform ((ang + 0.005) `mod''` (2*pi))

data ShaderEnvironment = ShaderEnvironment
  { primitives :: PrimitiveArray Triangles (MeshArray (ArrayOf (V3 Float)))
  , rasterOptions :: (Side, ViewPort, DepthRange)
  }

-- Project the cube's positions and normals with ModelViewProjection matrix
proj :: V4 (V4 VFloat)
  -> V3 (V3 VFloat)
  -> MeshVertex a
  -> (V4 VFloat, (V3 FlatVFloat, a))
proj modelViewProj normMat MeshVertex{..} = let
  V3 px py pz = meshPrimPosition
  in (modelViewProj !* V4 px py pz 1, (fmap Flat $ normMat !* meshPrimNormal, meshPrimData))

-- Set color from sampler and apply directional light
light :: (Num a, Floating a) => V3 a -> V3 a -> V4 a
light normal col = V4 r g b 1 * pure (normal `dot` V3 (-0.577) (-0.577) 0.577)
  where 
    gamma = 2.2
    V3 r g b = col ** gamma

word32Color :: RGBA -> V3 Float
word32Color (RGBA r g b _) = V3 r' g' b'
  where 
    r' = fromIntegral r / 255.0
    g' = fromIntegral g / 255.0
    b' = fromIntegral b / 255.0