module Data.Voxel.Viewer.Shader.Phong(
    ShaderEnvironment(..)
  , MatrixUniform
  , pipelineShader
  , renderScene
  ) where 

import Control.Lens ((^.))
import Data.Voxel.GPipe.Mesh
import Graphics.GPipe
import Data.Vector (Vector)
import Data.Voxel.Viewer.Camera
import Data.Voxel.Viewer.Scene
import Reflex 

import qualified Graphics.GPipe.Context.GLFW as GLFW
import qualified Data.Vector as GV

data ShaderEnvironment = ShaderEnvironment
  { primitives :: PrimitiveArray Triangles (MeshArray (ArrayOf (V3 Float)))
  , rasterOptions :: (Side, ViewPort, DepthRange)
  }

-- | Buffer where we keep MVP matrix and rotation matrix for normals
type MatrixUniform os = Buffer os (Uniform (V4 (B4 Float), V3 (B3 Float)))

-- | Simple phong shading with directed light
pipelineShader ::  Window os RGBAFloat Depth -> MatrixUniform os -> Shader os ShaderEnvironment ()
pipelineShader win uniform = do
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

renderScene :: Window os RGBAFloat Depth
  -> (ShaderEnvironment -> Render os ())
  -> Vector (SceneModel os)
  -> MatrixUniform os
  -> Camera Float
  -> Float
  -> Int 
  -> ContextT GLFW.Handle os (SpiderHost Global) ()
renderScene win shader makePrimitives uniform camera ang lod = do
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

