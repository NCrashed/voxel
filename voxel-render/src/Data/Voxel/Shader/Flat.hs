{-# LANGUAGE Arrows #-}
module Data.Voxel.Shader.Flat(
    Region(..)
  , Rect(..)
  , newRectBuffers
  , writeRectBuffers
  , rectBuffers
  , rectBufferArray
  , FlatEnv(..)
  , MatrixUniform
  , FlatContext(..)
  , newFlatContext
  , flatShader
  , LoadedRects
  , renderModel
  ) where 

import Control.Arrow
import Control.Lens ((^.))
import Control.Monad 
import Control.Monad.IO.Class 
import Data.Vector (Vector)
import Data.Voxel.App (SpiderCtx)
import Data.Word
import GHC.Generics (Generic)
import Graphics.GPipe

import qualified Data.Vector as V 

-- | Region is set by two points
data Region = Region !(V2 Float) !(V2 Float)
  deriving (Generic, Show)

-- | Get points of region starting from top left corner 
-- and going clockwise.
--
-- 1 -- 4 
-- |    |
-- 2 -- 3 
regionPoints :: Region -> [V2 Float]
regionPoints (Region (V2 x1 y1) (V2 x2 y2)) = [
    V2 x1 y1
  , V2 x1 y2 
  , V2 x2 y2 
  , V2 x2 y1 
  ]

-- | Get indecies to describe 2 triangles of region 
-- 
-- 1 ----- 4 
-- | *   2 |
-- |   *   |
-- | 1   * |
-- 2 ----- 3 
regionIndecies :: [Word32]
regionIndecies = [
    0, 1, 2 
  , 0, 2, 3
  ]

-- | Rect to draw
data Rect = Rect {
  -- | Position in screen
  rectPos   :: !Region
  -- | Position in atlas
, rectUv    :: !Region
  -- | Overlay level (higher is closer to camera)
, rectLevel :: !Int
} deriving (Generic, Show)

-- | Convert abastract overlay level to concrete Z value
rectZPos :: Rect -> Float 
rectZPos Rect{..} = - fromIntegral rectLevel / 255.0

-- | Structure that holds all buffers for rects
data RectBuffers os = RectBuffers {
-- | Holds triangles vertecies
  rectBuffPositions :: !(Buffer os (B3 Float))
-- | Holds triangles uv coords
, rectBuffUvs       :: !(Buffer os (B2 Float))
-- | Holds triples of indecies, each index corresponds to elements
-- from the previous buffers.
, rectBuffIndecies  :: !(Buffer os (B Word32))
} deriving (Eq, Generic)

-- | Create new buffers with given capacity (count of rects)
newRectBuffers :: (MonadIO m, ContextHandler ctx)
  => Int -- ^ Count of rects
  -> ContextT ctx os m (RectBuffers os)
newRectBuffers n = RectBuffers
  <$> newBuffer nverts
  <*> newBuffer nverts
  <*> newBuffer (ntrigs * 3)
  where 
    nverts = 4 * n 
    ntrigs = 2 * n 

-- | Write contents of rects to the given buffer. Note that size of buffer should
-- match the vector.
writeRectBuffers :: (MonadIO m, ContextHandler ctx)
  => Vector Rect
  -> RectBuffers os
  -> ContextT ctx os m ()
writeRectBuffers vs RectBuffers{..} = V.forM_ (V.indexed vs) $ \(i, r@Rect{..}) -> do
  let z = rectZPos r
  let poses = (\(V2 x y) -> V3 x y z) <$> regionPoints rectPos 
  guardedWrite "positions" (i*4) rectBuffPositions poses
  guardedWrite "uvs" (i*4) rectBuffUvs $ regionPoints rectUv
  guardedWrite "indicies" (i*2*3) rectBuffIndecies regionIndecies
  where 
    guardedWrite name offset buff vec = do
      when (bufferLength buff < offset + length vec) $ 
        fail $ name ++ " buffer size " ++ 
          show (bufferLength buff) ++ 
          ", but expected " ++ (show $ offset + length vec)
      writeBuffer buff offset vec

-- | Create new buffers and fill it with vertecies from rects
rectBuffers :: forall m ctx os . (MonadIO m, ContextHandler ctx)
  => Vector Rect
  -> ContextT ctx os m (RectBuffers os)
rectBuffers vs = do
  when (V.null vs) $ fail "Cannot fill rect buffers with no vertecies!"
  bs <- newRectBuffers (V.length vs)
  writeRectBuffers vs bs
  pure bs

-- | Convert rect buffers to rect vertex arrays
rectBufferArray :: RectBuffers os -> Render os (PrimitiveArray Triangles RectVertexArr)
rectBufferArray RectBuffers{..} = do
  p :: VertexArray () (B3 Float) <- newVertexArray rectBuffPositions
  u :: VertexArray () (B2 Float) <- newVertexArray rectBuffUvs
  i :: IndexArray <- newIndexArray rectBuffIndecies Nothing
  pure $ toPrimitiveArrayIndexed TriangleList i $ zipVertices RectVertexArr p u

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
sampleTex sampler uv = sample2D sampler SampleAuto Nothing Nothing uv

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

