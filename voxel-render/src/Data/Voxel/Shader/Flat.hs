module Data.Voxel.Shader.Flat(
    FlatEnv(..)
  , MatrixUniform
  , FlatContext(..)
  , newFlatContext
  , flatShader
  , LoadedRects
  , renderModel
  , module Data.Voxel.Rect 
  , module Data.Voxel.Rect.Buffer 
  ) where 

import Control.Arrow
import Control.Lens ((^.))
import Control.Monad 
import Control.Monad.IO.Class 
import Data.Vector (Vector)
import Data.Voxel.App (SpiderCtx)
import Data.Voxel.Rect 
import Data.Voxel.Rect.Buffer 
import Data.Word
import GHC.Generics (Generic)
import Graphics.GPipe

import qualified Data.Vector as V 

data FlatEnv os = FlatEnv
  { primitives :: PrimitiveArray Triangles RectVertexArr
  , rasterOptions :: (Side, ViewPort, DepthRange)
  , texture :: (Texture2D os (Format RGBAFloat), SamplerFilter RGBAFloat, (EdgeMode2, BorderColor RGBAFloat))
  }

-- | Buffer where we keep MVP matrix (no rotation is expected)
type MatrixUniform os = Buffer os (Uniform (V4 (B4 Float)))

-- | Simple flat shading for render1ing textured quads
flatShader :: Window os RGBAFloat Depth -> MatrixUniform os -> Shader os (FlatEnv os) ()
flatShader win uniform = do
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
sampleTex sampler uv = V4 r g b a
  where 
    gamma = 2.2
    col@(V4 _ _ _ a) = sample2D sampler SampleAuto Nothing Nothing uv
    V4 r g b _ = col ** gamma

-- | Environment that is required to draw a single frame
data FlatContext os = FlatContext {
  -- | Target window to render to 
  renderWindow :: !(Window os RGBAFloat Depth)
  -- | Buffer with MVP matrix
, renderMatrix :: !(MatrixUniform os)
  -- | Compiled shader to render with 
, renderShader :: !(CompiledShader os (FlatEnv os))
  -- | Loaded texture to tile the rect with
, renderTexture :: !(Texture2D os (Format RGBAFloat))
}

-- | Create new renderer context for given window
newFlatContext :: 
     Window os RGBAFloat Depth 
  -> Texture2D os (Format RGBAFloat)
  -> SpiderCtx os (FlatContext os)
newFlatContext win tex = do 
  matBuffer <- newBuffer 1
  shader <- compileShader $ flatShader win matBuffer 
  pure FlatContext {
      renderWindow = win 
    , renderShader = shader 
    , renderMatrix = matBuffer 
    , renderTexture = tex
    }

-- | Loaded model to GPU to render 
type LoadedRects os = Render os (PrimitiveArray Triangles RectVertexArr)
    
renderModel :: FlatContext os 
  -> LoadedRects os
  -> SpiderCtx os ()
renderModel FlatContext{..} primities = do
  -- Write this frames uniform value
  size@(V2 w h) <- getFrameBufferSize renderWindow
  let aspect = fromIntegral w / fromIntegral h
  let viewProjMat = ortho 0.0 aspect 0.0 1.0 0.0 100.0
  writeBuffer renderMatrix 0 [viewProjMat]

  -- Render the frame and present the results
  render $ do
    clearWindowColor renderWindow 0 -- Black
    clearWindowDepth renderWindow 1 -- Far plane
    prims <- primities
    renderShader $ FlatEnv {
      primitives = prims,
      rasterOptions = (FrontAndBack, ViewPort 0 size, DepthRange 0 1),
      texture = (renderTexture, SamplerNearest, (V2 ClampToEdge ClampToEdge, V4 0.0 0.0 0.0 0.0))
    }
  swapWindowBuffers renderWindow

