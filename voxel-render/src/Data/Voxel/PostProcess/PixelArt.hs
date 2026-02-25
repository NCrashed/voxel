{-# LANGUAGE NoImplicitPrelude #-}
-- | Pixel-art post-processing pipeline that renders at low resolution
-- and upscales with point filtering for crisp blocky pixels.
--
-- Pipeline flow:
--   Scene → [High-Res Texture] → Downsample → [Low-Res Texture] → Upscale → Window
--
-- Future-ready for color quantization step between downsample and upscale.
module Data.Voxel.PostProcess.PixelArt(
    -- * Configuration
    PixelArtConfig(..)
  , defaultPixelArtConfig
    -- * Pipeline context
  , PixelArtContext(..)
  , newPixelArtContext
    -- * Rendering
  , getHighResTextures
  , executePixelArtPipeline
    -- * Utilities
  , computeLowResSize
  ) where

import Data.Voxel.App (SpiderCtx)
import Data.Voxel.PostProcess.Blit
import Graphics.GPipe
import Linear
import Prelude hiding ((<*))

-- | Configuration for pixel-art rendering
data PixelArtConfig = PixelArtConfig
  { paDownscaleFactor :: !Int
    -- ^ Factor to downsample by (e.g., 4 = 1/4 resolution)
  , paHighResSize     :: !(V2 Int)
    -- ^ High-resolution render target size (typically window size)
  } deriving (Show, Eq)

-- | Default configuration: 4x downscale at 800x600
defaultPixelArtConfig :: PixelArtConfig
defaultPixelArtConfig = PixelArtConfig
  { paDownscaleFactor = 4
  , paHighResSize = V2 800 600
  }

-- | Pipeline context holding all resources for pixel-art post-processing
data PixelArtContext os = PixelArtContext
  { paConfig         :: !PixelArtConfig
    -- ^ Pipeline configuration
  , paHighResTex     :: !(Texture2D os (Format RGBAFloat))
    -- ^ High-resolution color render target
  , paHighResDepth   :: !(Texture2D os (Format Depth))
    -- ^ High-resolution depth render target
  , paLowResTex      :: !(Texture2D os (Format RGBAFloat))
    -- ^ Low-resolution intermediate texture (after downsample)
  , paBlitCtx        :: !(BlitContext os)
    -- ^ Blit context for final upscale to window
  , paBlitToImageCtx :: !(BlitToImageContext os)
    -- ^ Blit context for downsample to low-res texture
  }

-- | Compute low-resolution size from high-res and downscale factor
computeLowResSize :: PixelArtConfig -> V2 Int
computeLowResSize PixelArtConfig{..} =
  let V2 w h = paHighResSize
      factor = max 1 paDownscaleFactor
  in V2 (max 1 (w `div` factor)) (max 1 (h `div` factor))

-- | Create pixel-art pipeline context
newPixelArtContext
  :: Window os RGBAFloat Depth
  -> PixelArtConfig
  -> SpiderCtx os (PixelArtContext os)
newPixelArtContext win config = do
  let V2 highW highH = paHighResSize config
      V2 lowW lowH = computeLowResSize config

  -- Create high-resolution render targets
  highResTex <- newTexture2D RGBA16F (V2 highW highH) 1
  highResDepth <- newTexture2D Depth16 (V2 highW highH) 1

  -- Create low-resolution texture for downsampled result
  lowResTex <- newTexture2D RGBA16F (V2 lowW lowH) 1

  -- Create blit contexts
  blitCtx <- newBlitContext win
  blitToImageCtx <- newBlitToImageContext

  pure PixelArtContext
    { paConfig = config
    , paHighResTex = highResTex
    , paHighResDepth = highResDepth
    , paLowResTex = lowResTex
    , paBlitCtx = blitCtx
    , paBlitToImageCtx = blitToImageCtx
    }

-- | Get high-resolution render target textures for scene rendering.
-- Returns (colorTexture, depthTexture) - pass these to your scene render function.
getHighResTextures
  :: PixelArtContext os
  -> (Texture2D os (Format RGBAFloat), Texture2D os (Format Depth))
getHighResTextures PixelArtContext{..} = (paHighResTex, paHighResDepth)

-- | Execute the pixel-art post-processing pipeline.
-- This performs:
--   1. Downsample high-res texture to low-res using point filtering
--   2. Upscale low-res texture to window using point filtering
--
-- Note: Scene must already be rendered to high-res images before calling this.
-- GPipe requires texture sampling and rendering to same texture in different
-- render blocks, so we must use separate render calls.
executePixelArtPipeline
  :: PixelArtContext os
  -> Window os RGBAFloat Depth
  -> SpiderCtx os ()
executePixelArtPipeline PixelArtContext{..} win = do
  let lowResSize = computeLowResSize paConfig
      V2 highW highH = paHighResSize paConfig

  -- Step 1: Downsample high-res to low-res texture
  -- This uses point filtering to sample from high-res, creating crisp pixels
  render $ do
    lowResImg <- getTexture2DImage paLowResTex 0
    clearImageColor lowResImg 0
    blitToImage paBlitToImageCtx paHighResTex lowResImg lowResSize

  -- Step 2: Upscale low-res to window using point filtering
  -- The low-res pixels will be blocky due to nearest-neighbor sampling
  blitToWindow paBlitCtx win paLowResTex (V2 highW highH)

  swapWindowBuffers win
