{-# LANGUAGE NoImplicitPrelude #-}
-- | Pixel-art post-processing pipeline that renders at low resolution
-- and upscales with point filtering for crisp blocky pixels.
--
-- Pipeline flow:
--   Scene → [High-Res Texture] → Downsample → [Low-Res Texture] → [Quantize] → Upscale → Window
--
-- The optional quantization step uses CIE Lab color space for perceptually
-- uniform palette matching.
module Data.Voxel.PostProcess.PixelArt(
    -- * Configuration
    PixelArtConfig(..)
  , defaultPixelArtConfig
    -- * Pipeline context
  , PixelArtContext(..)
  , newPixelArtContext
  , updatePixelArtPalette
  , updatePixelArtDitherStrength
    -- * Rendering
  , getHighResTextures
  , executePixelArtPipeline
  , executePixelArtPipelineEx
    -- * Utilities
  , computeLowResSize
    -- * Re-exports for convenience
  , Palette
  , defaultPalette
  , loadPaletteFromImage
  ) where

import Data.Voxel.App (SpiderCtx)
import Data.Voxel.PostProcess.Blit
import Data.Voxel.PostProcess.ColorQuantize
import Graphics.GPipe
import Linear
import Prelude hiding ((<*))

import qualified Data.Vector as V

-- | Configuration for pixel-art rendering
data PixelArtConfig = PixelArtConfig
  { paDownscaleFactor :: !Int
    -- ^ Factor to downsample by (e.g., 4 = 1/4 resolution)
  , paHighResSize     :: !(V2 Int)
    -- ^ High-resolution render target size (typically window size)
  , paPalette         :: !(Maybe Palette)
    -- ^ Optional palette for color quantization. When 'Just', enables
    -- CIE Lab color quantization to reduce colors to the palette.
  , paDitherStrength  :: !Float
    -- ^ Dithering strength for ordered dithering (0 = off, 1 = normal)
  } deriving (Show, Eq)

-- | Default configuration: 4x downscale at 800x600, no palette
defaultPixelArtConfig :: PixelArtConfig
defaultPixelArtConfig = PixelArtConfig
  { paDownscaleFactor = 4
  , paHighResSize = V2 800 600
  , paPalette = Nothing
  , paDitherStrength = 1.0
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
  , paQuantizedTex   :: !(Maybe (Texture2D os (Format RGBAFloat)))
    -- ^ Quantized texture (after color quantization), if enabled
  , paQuantizeCtx    :: !(Maybe (QuantizeContext os))
    -- ^ Quantization context, if palette is enabled
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

  -- Create quantization resources if palette is provided
  (quantizedTex, quantizeCtx) <- case paPalette config of
    Nothing -> pure (Nothing, Nothing)
    Just palette -> do
      -- Create texture for quantized output
      qTex <- newTexture2D RGBA16F (V2 lowW lowH) 1
      -- Create quantization context
      qCtx <- newQuantizeContext $ QuantizeConfig
        { qcPalette = palette
        , qcDitherStrength = paDitherStrength config
        }
      pure (Just qTex, Just qCtx)

  -- Create blit contexts
  blitCtx <- newBlitContext win
  blitToImageCtx <- newBlitToImageContext

  pure PixelArtContext
    { paConfig = config
    , paHighResTex = highResTex
    , paHighResDepth = highResDepth
    , paLowResTex = lowResTex
    , paQuantizedTex = quantizedTex
    , paQuantizeCtx = quantizeCtx
    , paBlitCtx = blitCtx
    , paBlitToImageCtx = blitToImageCtx
    }

-- | Update the palette used for color quantization.
-- Does nothing if quantization is not enabled (paPalette was Nothing).
updatePixelArtPalette
  :: PixelArtContext os
  -> Palette
  -> SpiderCtx os ()
updatePixelArtPalette PixelArtContext{..} palette =
  case paQuantizeCtx of
    Nothing -> pure ()
    Just qCtx -> updateQuantizePalette qCtx palette

-- | Update the dithering strength used for color quantization.
-- Does nothing if quantization is not enabled (paPalette was Nothing).
-- Values: 0.0 = no dithering, 1.0 = full dithering strength.
updatePixelArtDitherStrength
  :: PixelArtContext os
  -> Float
  -> SpiderCtx os ()
updatePixelArtDitherStrength PixelArtContext{..} strength =
  case paQuantizeCtx of
    Nothing -> pure ()
    Just qCtx -> updateDitherStrength qCtx strength

-- | Get high-resolution render target textures for scene rendering.
-- Returns (colorTexture, depthTexture) - pass these to your scene render function.
getHighResTextures
  :: PixelArtContext os
  -> (Texture2D os (Format RGBAFloat), Texture2D os (Format Depth))
getHighResTextures PixelArtContext{..} = (paHighResTex, paHighResDepth)

-- | Execute the pixel-art post-processing pipeline.
-- This performs:
--   1. Downsample high-res texture to low-res using point filtering
--   2. (Optional) Quantize colors to palette using CIE Lab color space
--   3. Upscale low-res/quantized texture to window using point filtering
--
-- Note: Scene must already be rendered to high-res images before calling this.
-- GPipe requires texture sampling and rendering to same texture in different
-- render blocks, so we must use separate render calls.
executePixelArtPipeline
  :: PixelArtContext os
  -> Window os RGBAFloat Depth
  -> SpiderCtx os ()
executePixelArtPipeline ctx win = executePixelArtPipelineEx ctx win True

-- | Execute the pixel-art post-processing pipeline with explicit quantization control.
-- Same as 'executePixelArtPipeline' but allows disabling quantization at runtime.
executePixelArtPipelineEx
  :: PixelArtContext os
  -> Window os RGBAFloat Depth
  -> Bool  -- ^ Whether to apply palette quantization (if available)
  -> SpiderCtx os ()
executePixelArtPipelineEx PixelArtContext{..} win applyQuantization = do
  let lowResSize = computeLowResSize paConfig
      V2 highW highH = paHighResSize paConfig

  -- Step 1: Downsample high-res to low-res texture
  -- This uses point filtering to sample from high-res, creating crisp pixels
  render $ do
    lowResImg <- getTexture2DImage paLowResTex 0
    clearImageColor lowResImg 0
    blitToImage paBlitToImageCtx paHighResTex lowResImg lowResSize

  -- Step 2: Quantize colors to palette (if enabled and requested)
  -- Converts each pixel to CIE Lab, finds closest palette color
  let doQuantize = applyQuantization
  case (paQuantizeCtx, paQuantizedTex) of
    (Just qCtx, Just qTex) | doQuantize -> render $ do
      quantImg <- getTexture2DImage qTex 0
      clearImageColor quantImg 0
      quantizeToImage qCtx paLowResTex quantImg lowResSize
    _ -> pure ()

  -- Step 3: Upscale to window using point filtering
  -- Use quantized texture if quantization was applied, otherwise use low-res
  let srcTex = if doQuantize
               then maybe paLowResTex id paQuantizedTex
               else paLowResTex
  blitToWindow paBlitCtx win srcTex (V2 highW highH)

  swapWindowBuffers win
