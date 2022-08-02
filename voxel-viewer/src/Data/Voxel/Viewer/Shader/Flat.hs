{-# LANGUAGE Arrows #-}
module Data.Voxel.Viewer.Shader.Flat(
    ShaderEnvironment(..)
  , MatrixUniform
  , pipelineShader
  , renderScene
  ) where 

import Linear 
import Control.Arrow
import Control.Lens ((^.))
import Graphics.GPipe
import Reflex 

import qualified Graphics.GPipe.Context.GLFW as GLFW

-- | Structure that is used inside shader primitive array
data RectVertexArr = RectVertexArr {
  rectArrPosition :: !(B3 Float)
, rectArrUv       :: !(B2 Float)
}

-- | Structure that is used inside vertex shader
data RectVertex = RectVertex {
  rectPrimPosition :: !(V3 VFloat)
, rectPrimUv       :: !(V2 VFloat)
}

instance VertexInput RectVertexArr where
  type VertexFormat RectVertexArr = RectVertex
  toVertex = proc ~(RectVertexArr p u) -> do
    p' <- toVertex -< p
    u' <- toVertex -< u
    returnA -< RectVertex p' u'
    
data ShaderEnvironment os = ShaderEnvironment
  { primitives :: PrimitiveArray Triangles RectVertexArr
  , rasterOptions :: (Side, ViewPort, DepthRange)
  , texture :: (Texture2D os (Format RGBAFloat), SamplerFilter RGBAFloat, (EdgeMode2, BorderColor RGBAFloat))
  }

-- | Buffer where we keep MVP matrix (no rotation is expected)
type MatrixUniform os = Buffer os (Uniform (V4 (B4 Float)))

-- | Simple flat shading for rendering textured quads
pipelineShader :: Window os RGBAFloat Depth -> MatrixUniform os -> Shader os (ShaderEnvironment os) ()
pipelineShader win uniform = do
  sides <- toPrimitiveStream primitives
  modelViewProj <- getUniform (const (uniform, 0))
  let projectedSides = proj modelViewProj <$> sides

  sampler <- newSampler2D texture
  fragNormalsUV <- rasterize rasterOptions projectedSides
  let litFrags = sampleTex sampler <$> fragNormalsUV
      litFragsWithDepth = withRasterizedInfo
          (\a x -> (a, rasterizedFragCoord x ^. _z)) litFrags
      colorOption = ContextColorOption NoBlending (pure True)
      depthOption = DepthOption Less True

  drawWindowColorDepth (const (win, colorOption, depthOption)) litFragsWithDepth

-- Project positions with ModelViewProjection matrix
proj :: V4 (V4 VFloat)
  -> RectVertex
  -> (V4 VFloat, V2 FlatVFloat)
proj modelViewProj RectVertex{..} = let
  V3 px py pz = rectPrimPosition
  in (modelViewProj !* V4 px py pz 1, Flat <$> rectPrimUv)

-- Set color from sampler
sampleTex :: Sampler2D (Format RGBAFloat) -> V2 FFloat -> V4 FFloat
sampleTex sampler uv = sample2D sampler SampleAuto Nothing Nothing uv

-- | Loaded model to GPU to render 
type LoadedRects os = Render
       os (PrimitiveArray Triangles RectVertexArr)
    
renderScene :: Window os RGBAFloat Depth
  -> (ShaderEnvironment os -> Render os ())
  -> LoadedRects os
  -> Texture2D os (Format RGBAFloat)
  -> MatrixUniform os
  -> ContextT GLFW.Handle os (SpiderHost Global) ()
renderScene win shader primities tex uniform = do
  -- Write this frames uniform value
  size@(V2 w h) <- getFrameBufferSize win
  let aspect = fromIntegral w / fromIntegral h
  let viewProjMat = ortho 0.0 aspect 0.0 1.0 0.0 100.0
  writeBuffer uniform 0 [viewProjMat]

  -- Render the frame and present the results
  render $ do
    clearWindowColor win 0 -- Black
    clearWindowDepth win 1 -- Far plane
    prims <- primities
    shader $ ShaderEnvironment {
      primitives = prims,
      rasterOptions = (FrontAndBack, ViewPort 0 size, DepthRange 0 1),
      texture = (tex, SamplerNearest, (V2 ClampToEdge ClampToEdge, V4 0.0 0.0 0.0 0.0))
    }
  swapWindowBuffers win

