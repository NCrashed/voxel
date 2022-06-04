module Data.Voxel.Viewer(
    runViewer
  ) where

import Control.Lens ((^.))
import Control.Monad
import Data.MagicaVoxel.Types (RGBA(..))
import Data.Maybe
import Data.Proxy
import Data.Vector (Vector)
import Data.Voxel.Generator
import Data.Voxel.GPipe.Mesh
import Data.Voxel.Grid.Unbox.Polygon
import Data.Voxel.MagicaVoxel (convertMagica)
import Data.Voxel.Viewer.Camera
import Graphics.GPipe

import qualified Data.MagicaVoxel as MV 
import qualified Data.Vector as GV
import qualified Data.Voxel.Grid.Unbox as G
import qualified Data.Voxel.Grid.Unbox.Mutable as GM
import qualified Data.Voxel.Mesh as M
import qualified Data.Voxel.Mesh.Lod as L
import qualified Graphics.GPipe.Context.GLFW as GLFW
import qualified Math.Noise as N 

runViewer :: IO ()
runViewer = do
  runContextT GLFW.defaultHandleConfig $ do

    let initWidth = 800
    let initHeight = 600
    let wcfg = (GLFW.defaultWindowConfig "voxel viewer") {
            GLFW.configWidth = initWidth
          , GLFW.configHeight = initHeight
          }
    win <- newWindow (WindowFormatColorDepth RGBA8 Depth16) wcfg
    voxModel <- either (fail . ("Vox loading: " ++)) pure =<< MV.parseFile "../MagicaVoxel-vox/test/harvester_full.vox"
    rawModel <- either (fail . ("Vox convert: " ++)) pure $ convertMagica voxModel
    -- Create vertex data buffers
    let model :: G.VoxelGrid (V3 Float)
        -- model = generateMap 
        model = G.map word32Color rawModel
    let renderModel :: L.LodMesh (V3 Float)
        renderModel = L.new TriangulateTriangles model
    buffers <- traverse meshBuffers $ L.allLods renderModel
    -- Make a Render action that returns a PrimitiveArray for the cube
    let makeSingle = meshBufferArray (Proxy :: Proxy (V3 Float)) TriangleList
    let makePrimitives = GV.fromList $ makeSingle <$> buffers
          
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

    let camera = Camera {
          cameraPosition = - V3 0 0 5
        , cameraRotation = axisAngle (V3 1 0 0) (-pi/3)
        , cameraAngle = (pi/9)
        , cameraAspect = (fromIntegral initWidth / fromIntegral initHeight)
        , cameraNear = 1 
        , cameraFar = 100
        }
    -- Run the loop
    loop win shader makePrimitives uniform camera 0 0

loop :: Window os RGBAFloat Depth
  -> (ShaderEnvironment -> Render os ())
  -> Vector (Render
       os (PrimitiveArray Triangles (MeshArray (ArrayOf (V3 Float)))))
  -> Buffer os (Uniform (V4 (B4 Float), V3 (B3 Float)))
  -> Camera Float
  -> Float
  -> Int 
  -> ContextT GLFW.Handle os IO ()
loop win shader makePrimitives uniform !camera !ang !lod = do
  -- Write this frames uniform value
  size@(V2 w h) <- getFrameBufferSize win
  let newCam = camera { cameraAspect = (fromIntegral w / fromIntegral h) }
  let modelRot = fromQuaternion (axisAngle (V3 0 0 1) ang)
      modelMat = mkTransformationMat modelRot (pure 0) !*! mkTransformationMat identity (-V3 0.5 0.5 0)
      viewProjMat = cameraProjMat newCam !*! cameraViewMat newCam !*! modelMat
      normMat = modelRot
  writeBuffer uniform 0 [(viewProjMat, normMat)]

  -- Render the frame and present the results
  render $ do
    clearWindowColor win 0 -- Black
    clearWindowDepth win 1 -- Far plane
    prims <- makePrimitives GV.! lod
    shader $ ShaderEnvironment prims (FrontAndBack, ViewPort 0 size, DepthRange 0 1)
  swapWindowBuffers win

  closeRequested <- GLFW.windowShouldClose win
  let getPressed i k = do 
        ms <- GLFW.getKey win k
        pure $ if ms == Just GLFW.KeyState'Pressed
          then Just i else Nothing 
      getPressedLod ks = do 
        mks <- traverse (uncurry getPressed) $ [0 ..] `zip` ks 
        pure $ case catMaybes mks of 
          i:_ -> min (GV.length makePrimitives - 1) i 
          _ -> lod   
  newlod <- getPressedLod [GLFW.Key'0, GLFW.Key'1, GLFW.Key'2, GLFW.Key'3, GLFW.Key'4, GLFW.Key'5, GLFW.Key'6]
  unless (fromMaybe False closeRequested) $
    loop win shader makePrimitives uniform newCam ((ang + 0.005) `mod''` (2*pi)) newlod

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