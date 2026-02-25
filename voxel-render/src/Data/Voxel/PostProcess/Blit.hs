{-# LANGUAGE NoImplicitPrelude #-}
-- | Fullscreen blit shader for texture-to-texture and texture-to-window rendering.
-- Supports point-filtered sampling for crisp pixel-art effects.
module Data.Voxel.PostProcess.Blit(
    -- * Blit to window
    BlitEnv(..)
  , BlitContext(..)
  , newBlitContext
  , blitToWindow
    -- * Blit to image
  , BlitToImageEnv(..)
  , BlitToImageContext(..)
  , newBlitToImageContext
  , blitToImage
    -- * Fullscreen quad utilities
  , fullscreenQuad
  , fullscreenQuadBuffers
  ) where

import Control.Lens ((^.))
import Data.Voxel.App (SpiderCtx)
import Data.Voxel.Rect
import Data.Voxel.Rect.Buffer
import Graphics.GPipe
import Linear
import Prelude hiding ((<*))

import qualified Data.Vector as V

-- | Environment for blit shader rendering to window
data BlitEnv os = BlitEnv
  { blitPrimitives    :: !(PrimitiveArray Triangles RectVertexArr)
    -- ^ Fullscreen quad primitives
  , blitRasterOptions :: !(Side, ViewPort, DepthRange)
    -- ^ Rasterization options
  , blitSourceTexture :: !( Texture2D os (Format RGBAFloat)
                          , SamplerFilter RGBAFloat
                          , (EdgeMode2, BorderColor RGBAFloat)
                          )
    -- ^ Source texture with sampler settings (use Nearest for point filtering)
  }

-- | Context for blit shader operations to window
data BlitContext os = BlitContext
  { blitCompiled  :: !(CompiledShader os (BlitEnv os))
    -- ^ Compiled blit shader
  , blitQuadBuffs :: !(RectBuffers os)
    -- ^ Fullscreen quad vertex buffers
  }

-- | Blit shader that samples a texture and outputs to window
blitShader
  :: Window os RGBAFloat Depth
  -> Shader os (BlitEnv os) ()
blitShader win = do
  verts <- toPrimitiveStream blitPrimitives

  -- Transform fullscreen quad positions to clip space
  let projectedVerts = projectFullscreen <$> verts

  -- Create sampler for source texture
  sourceSampler <- newSampler2D blitSourceTexture

  -- Rasterize
  fragData <- rasterize blitRasterOptions projectedVerts

  -- Sample source texture
  let sampledFrags = sampleSource sourceSampler <$> fragData
      sampledFragsWithDepth = withRasterizedInfo
        (\col _ -> (col, 0)) sampledFrags
      colorOption = ContextColorOption NoBlending (pure True)
      depthOption = DepthOption Always True

  drawWindowColorDepth (const (win, colorOption, depthOption)) sampledFragsWithDepth

-- | Project fullscreen vertex to clip space
-- Input positions are in NDC (-1 to 1)
projectFullscreen :: RectVertex -> (V4 VFloat, V2 VFloat)
projectFullscreen RectVertex{..} =
  let V3 x y z = rectPrimPosition
      clipPos = V4 x y z 1
  in (clipPos, rectPrimUv)

-- | Sample source texture at UV coordinate
sampleSource :: Sampler2D (Format RGBAFloat) -> V2 FFloat -> V4 FFloat
sampleSource sampler uv = sample2D sampler SampleAuto Nothing Nothing uv

-- | Create new blit context for rendering to window
newBlitContext
  :: Window os RGBAFloat Depth
  -> SpiderCtx os (BlitContext os)
newBlitContext win = do
  quadBuffs <- fullscreenQuadBuffers
  shader <- compileShader $ blitShader win
  pure BlitContext
    { blitCompiled = shader
    , blitQuadBuffs = quadBuffs
    }

-- | Blit texture to window using point filtering
blitToWindow
  :: BlitContext os
  -> Window os RGBAFloat Depth
  -> Texture2D os (Format RGBAFloat)
  -> V2 Int          -- ^ Viewport size
  -> SpiderCtx os ()
blitToWindow BlitContext{..} win sourceTex viewportSize = do
  render $ do
    clearWindowColor win 0
    clearWindowDepth win 1
    prims <- rectBufferArray blitQuadBuffs

    blitCompiled $ BlitEnv
      { blitPrimitives = prims
      , blitRasterOptions = (FrontAndBack, ViewPort 0 viewportSize, DepthRange 0 1)
      , blitSourceTexture = ( sourceTex
                            , SamplerFilter Nearest Nearest Nearest Nothing
                            , (V2 ClampToEdge ClampToEdge, V4 0 0 0 0)
                            )
      }

--------------------------------------------------------------------------------
-- Blit to image (for intermediate passes)
--------------------------------------------------------------------------------

-- | Environment for blit shader rendering to image
data BlitToImageEnv os = BlitToImageEnv
  { blitToImagePrimitives    :: !(PrimitiveArray Triangles RectVertexArr)
    -- ^ Fullscreen quad primitives
  , blitToImageRasterOptions :: !(Side, ViewPort, DepthRange)
    -- ^ Rasterization options
  , blitToImageSource        :: !( Texture2D os (Format RGBAFloat)
                                 , SamplerFilter RGBAFloat
                                 , (EdgeMode2, BorderColor RGBAFloat)
                                 )
    -- ^ Source texture with sampler settings
  , blitToImageTarget        :: !(Image (Format RGBAFloat))
    -- ^ Target color image
  }

-- | Context for blit to image operations
data BlitToImageContext os = BlitToImageContext
  { blitToImageCompiled  :: !(CompiledShader os (BlitToImageEnv os))
    -- ^ Compiled shader
  , blitToImageQuadBuffs :: !(RectBuffers os)
    -- ^ Fullscreen quad vertex buffers
  }

-- | Blit shader that samples texture and outputs to image
blitToImageShader :: Shader os (BlitToImageEnv os) ()
blitToImageShader = do
  verts <- toPrimitiveStream blitToImagePrimitives

  let projectedVerts = projectFullscreen <$> verts

  sourceSampler <- newSampler2D blitToImageSource

  fragData <- rasterize blitToImageRasterOptions projectedVerts

  let sampledFrags = sampleSource sourceSampler <$> fragData
      -- ColorMask for RGBA is V4 Bool, UseBlending is False for no blending
      colorMask = V4 True True True True  -- Write all channels

  -- Use 'draw' to wrap DrawColors monad into Shader monad
  draw (const NoBlending) sampledFrags $ \colorVal ->
    drawColor (\env -> (blitToImageTarget env, colorMask, False)) colorVal

-- | Create new blit-to-image context
newBlitToImageContext :: SpiderCtx os (BlitToImageContext os)
newBlitToImageContext = do
  quadBuffs <- fullscreenQuadBuffers
  shader <- compileShader blitToImageShader
  pure BlitToImageContext
    { blitToImageCompiled = shader
    , blitToImageQuadBuffs = quadBuffs
    }

-- | Blit source texture to target image
blitToImage
  :: BlitToImageContext os
  -> Texture2D os (Format RGBAFloat)  -- ^ Source texture
  -> Image (Format RGBAFloat)         -- ^ Target image
  -> V2 Int                           -- ^ Target viewport size
  -> Render os ()
blitToImage BlitToImageContext{..} sourceTex targetImg viewportSize = do
  prims <- rectBufferArray blitToImageQuadBuffs

  blitToImageCompiled $ BlitToImageEnv
    { blitToImagePrimitives = prims
    , blitToImageRasterOptions = (FrontAndBack, ViewPort 0 viewportSize, DepthRange 0 1)
    , blitToImageSource = ( sourceTex
                          , SamplerFilter Nearest Nearest Nearest Nothing
                          , (V2 ClampToEdge ClampToEdge, V4 0 0 0 0)
                          )
    , blitToImageTarget = targetImg
    }

--------------------------------------------------------------------------------
-- Fullscreen quad utilities
--------------------------------------------------------------------------------

-- | Create a fullscreen quad rect covering NDC space (-1 to 1)
fullscreenQuad :: Rect
fullscreenQuad = Rect
  { rectPos = Region (V2 (-1) (-1)) (V2 1 1)
  , rectUv = Region (V2 0 0) (V2 1 1)
  , rectLevel = 0
  }

-- | Create fullscreen quad vertex buffers
fullscreenQuadBuffers :: SpiderCtx os (RectBuffers os)
fullscreenQuadBuffers = rectBuffers $ V.singleton fullscreenQuad
