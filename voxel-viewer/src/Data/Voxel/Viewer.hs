module Data.Voxel.Viewer(
    runViewer
  ) where

import Control.Lens ((^.))
import Control.Monad
import Data.Bits
import Data.Int
import Data.Maybe
import Data.Proxy
import Data.Voxel.GPipe.Mesh
import Data.Voxel.Grid.Unbox.Polygon
import Data.Word
import Debug.Trace
import Graphics.GPipe
import Data.Voxel.MagicaVoxel (convertMagica)

import qualified Data.MagicaVoxel as MV 
import qualified Data.Vector.Storable as V
import qualified Data.Voxel.Grid.Unbox as G
import qualified Data.Voxel.Grid.Unbox.Mutable as GM
import qualified Data.Voxel.Mesh as M
import qualified Graphics.GPipe.Context.GLFW as GLFW

runViewer :: IO ()
runViewer = do
  runContextT GLFW.defaultHandleConfig $ do

    let wcfg = (GLFW.defaultWindowConfig "voxel viewer") {
            GLFW.configWidth = 800
          , GLFW.configHeight = 600
          }
    win <- newWindow (WindowFormatColorDepth RGBA8 Depth16) wcfg
    voxModel <- either (fail . ("Vox loading: " ++)) pure =<< MV.parseFile "../MagicaVoxel-vox/test/teapot.vox"
    rawModel <- either (fail . ("Vox convert: " ++)) pure $ convertMagica voxModel
    -- Create vertex data buffers
    let model :: G.VoxelGrid (V3 Float)
        -- model = G.create $ do
        --   let sn = 12
        --   let sn2 = sn `div` 2
        --   let r = sn2
        --   let size@(V3 sx sy sz) = V3 sn sn sn
        --   g <- GM.new size
        --   let c = V3 1.0 0.4 0.5
        --   let is = [V3 x y z | x <- [0 .. sx-1], y <- [0 .. sy-1], z <- [0 .. sz-1], (x-sn2)*(x-sn2) + (y-sn2)*(y-sn2) + (z-sn2)*(z-sn2) <= r*r]
        --   mapM_ (\i@(V3 x y z) -> GM.write g i $ V3 0.5 0.4 0.5) is   
        --   pure g
        model = G.map word32Color rawModel
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
  let modelRot = fromQuaternion (axisAngle (V3 1 0.5 0.3) ang)
      modelMat = mkTransformationMat modelRot (pure 0)
      projMat = perspective (pi/9) (fromIntegral w / fromIntegral h) 1 100
      viewMat = mkTransformationMat identity (- V3 0 0 5)
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
light :: Num a => V3 a -> V3 a -> V4 a
light normal (V3 r g b) = V4 r g b 1 * pure (normal `dot` V3 0 0 1)

word32Color :: Word32 -> V3 Float
word32Color w = V3 r g b 
  where 
    w' :: Int = fromIntegral w
    r = (fromIntegral (w' .&. 0xFF) :: Float) / 255.0
    g = (fromIntegral (shiftR (w' .&. 0xFF00) 8) :: Float) / 255.0
    b = (fromIntegral (shiftR (w' .&. 0xFF0000) 16) :: Float) / 255.0
