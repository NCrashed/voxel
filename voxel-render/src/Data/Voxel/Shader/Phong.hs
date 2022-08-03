module Data.Voxel.Shader.Phong(
    ShaderEnvironment(..)
  , MatrixUniform
  , pipelineShader
  , RenderContext(..)
  , renderModel
  ) where 

import Control.Lens ((^.))
import Data.Voxel.Camera
import Data.Voxel.GPipe.Mesh
import Data.Voxel.Scene
import Data.Voxel.Transform 
import Graphics.GPipe
import Reflex 

import qualified Graphics.GPipe.Context.GLFW as GLFW

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

-- | Required arguments to render a frame
data RenderContext os = RenderContext {
  -- | Target window to render to 
  renderWindow :: !(Window os RGBAFloat Depth)
  -- | Buffer with MVP matrix
, renderMatrix :: !(MatrixUniform os)
  -- | View projection
, renderCamera :: !(Camera Float)
  -- | Compiled shader to render with 
, renderShader :: !(ShaderEnvironment -> Render os ())
}

-- | Render a single model in the screen
renderModel :: RenderContext os
  -> SceneModel os -- ^ Loaded scene model into memory 
  -> Transform -- ^ Model transformation 
  -> ContextT GLFW.Handle os (SpiderHost Global) ()
renderModel RenderContext{..} model transform = do
  -- Write this frames uniform value
  size@(V2 w h) <- getFrameBufferSize renderWindow
  let newCam = renderCamera { cameraAspect = (fromIntegral w / fromIntegral h) }
      modelMat = transformMatrix transform
      viewProjMat = cameraProjMat newCam !*! cameraViewMat newCam !*! modelMat
      normMat = fromQuaternion (transformRotation transform)
  writeBuffer renderMatrix 0 [(viewProjMat, normMat)]

  -- Render the frame and present the results
  render $ do
    clearWindowColor renderWindow 0 -- Black
    clearWindowDepth renderWindow 1 -- Far plane
    prims <- model
    renderShader $ ShaderEnvironment prims (FrontAndBack, ViewPort 0 size, DepthRange 0 1)
  swapWindowBuffers renderWindow

