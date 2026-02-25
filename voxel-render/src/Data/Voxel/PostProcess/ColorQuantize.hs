{-# LANGUAGE NoImplicitPrelude #-}
-- | CIE Lab color quantization shader for perceptually uniform palette matching.
--
-- The shader converts each pixel from sRGB to CIE Lab color space, finds the
-- closest palette color using Euclidean distance in Lab space (which correlates
-- well with human color perception), and outputs the corresponding sRGB color.
--
-- Color space conversion chain:
--   sRGB (input) -> Linear RGB -> CIE XYZ (D65) -> CIE Lab -> Find Closest -> sRGB (output)
--
-- The palette is stored in a 256x2 texture (supports up to 256 colors):
--   - Row 0 (y=0.25): sRGB values (R, G, B)
--   - Row 1 (y=0.75): Lab values (L in R, a in G, b in B)
module Data.Voxel.PostProcess.ColorQuantize(
    -- * Configuration
    QuantizeConfig(..)
  , defaultQuantizeConfig
  , Palette
  , defaultPalette
    -- * Palette loading
  , loadPaletteFromImage
    -- * Context
  , QuantizeContext(..)
  , newQuantizeContext
  , updateQuantizePalette
  , updateDitherStrength
    -- * Rendering
  , quantizeToImage
    -- * Color space conversion (CPU-side)
  , srgbToLab
  , labToSrgb
) where

import Data.Voxel.App (SpiderCtx)
import Data.Voxel.Rect
import Data.Voxel.Rect.Buffer
import Data.List (nubBy)
import Graphics.GPipe
import Linear
import Prelude hiding ((<*))

import qualified Codec.Picture as JP
import qualified Data.Vector as V

--------------------------------------------------------------------------------
-- Types and Configuration
--------------------------------------------------------------------------------

-- | Palette is a vector of sRGB colors in [0,1] range
type Palette = V.Vector (V3 Float)

-- | Configuration for color quantization
data QuantizeConfig = QuantizeConfig
  { qcPalette        :: !Palette
    -- ^ Up to 256 colors in sRGB [0,1] range
  , qcDitherStrength :: !Float
    -- ^ Ordered dithering strength (0 = no dithering, 1 = full dithering)
    -- Dithering helps distribute colors more evenly across the palette
    -- by adding a pattern-based offset before finding the nearest color.
    -- Typical values: 0.0 (off), 0.5 (subtle), 1.0 (strong)
  } deriving (Show, Eq)

-- | Default configuration with a retro 16-color palette and no dithering
defaultQuantizeConfig :: QuantizeConfig
defaultQuantizeConfig = QuantizeConfig
  { qcPalette = defaultPalette
  , qcDitherStrength = 0.0
  }

-- | A nice retro 16-color palette inspired by fantasy consoles
defaultPalette :: Palette
defaultPalette = V.fromList
  [ V3 0.07 0.07 0.12  -- 0: Dark blue-black
  , V3 0.20 0.16 0.28  -- 1: Dark purple
  , V3 0.35 0.24 0.35  -- 2: Purple
  , V3 0.55 0.35 0.44  -- 3: Mauve
  , V3 0.76 0.53 0.49  -- 4: Salmon
  , V3 0.93 0.76 0.65  -- 5: Peach
  , V3 0.98 0.95 0.82  -- 6: Cream
  , V3 0.67 0.85 0.65  -- 7: Light green
  , V3 0.38 0.65 0.52  -- 8: Green
  , V3 0.24 0.45 0.45  -- 9: Teal
  , V3 0.20 0.28 0.40  -- 10: Dark teal
  , V3 0.28 0.36 0.55  -- 11: Blue
  , V3 0.42 0.52 0.72  -- 12: Light blue
  , V3 0.62 0.70 0.82  -- 13: Pale blue
  , V3 0.45 0.40 0.32  -- 14: Brown
  , V3 0.72 0.60 0.45  -- 15: Tan
  ]

--------------------------------------------------------------------------------
-- Palette Loading from Image
--------------------------------------------------------------------------------

-- | Load a palette from an image file.
-- Extracts unique colors from the image and returns them as a palette.
-- Colors are limited to 16 (takes first 16 unique colors found).
-- Returns Left with error message on failure.
loadPaletteFromImage :: FilePath -> IO (Either String Palette)
loadPaletteFromImage path = do
  result <- JP.readImage path
  case result of
    Left err -> pure $ Left err
    Right dynamicImg -> pure $ Right $ extractPalette dynamicImg

-- | Extract unique colors from a dynamic image (up to 256 colors)
extractPalette :: JP.DynamicImage -> Palette
extractPalette dynamicImg = V.fromList $ take 256 uniqueColors
  where
    -- Convert to RGBA8 for uniform processing
    rgbaImg = JP.convertRGBA8 dynamicImg
    width = JP.imageWidth rgbaImg
    height = JP.imageHeight rgbaImg

    -- Extract all pixels as V3 Float (sRGB in [0,1])
    allPixels = [ pixelToV3 (JP.pixelAt rgbaImg x y)
                | y <- [0 .. height - 1]
                , x <- [0 .. width - 1]
                ]

    -- Remove duplicates (using approximate equality for floating point)
    uniqueColors = nubBy colorApproxEq allPixels

-- | Convert RGBA8 pixel to V3 Float in [0,1] range
pixelToV3 :: JP.PixelRGBA8 -> V3 Float
pixelToV3 (JP.PixelRGBA8 r g b _) = V3 (fromIntegral r / 255)
                                       (fromIntegral g / 255)
                                       (fromIntegral b / 255)

-- | Approximate equality for colors (within 1/256 tolerance)
colorApproxEq :: V3 Float -> V3 Float -> Bool
colorApproxEq (V3 r1 g1 b1) (V3 r2 g2 b2) =
  abs (r1 - r2) < tolerance &&
  abs (g1 - g2) < tolerance &&
  abs (b1 - b2) < tolerance
  where
    tolerance = 1.0 / 256.0

--------------------------------------------------------------------------------
-- CPU-side Color Space Conversion
--------------------------------------------------------------------------------

-- | Convert sRGB to linear RGB (gamma decode)
srgbToLinear :: Float -> Float
srgbToLinear c
  | c <= 0.04045 = c / 12.92
  | otherwise    = ((c + 0.055) / 1.055) ** 2.4

-- | Convert linear RGB to sRGB (gamma encode)
linearToSrgb :: Float -> Float
linearToSrgb c
  | c <= 0.0031308 = c * 12.92
  | otherwise      = 1.055 * (c ** (1/2.4)) - 0.055

-- | Convert linear RGB to CIE XYZ (D65 white point)
linearToXYZ :: V3 Float -> V3 Float
linearToXYZ (V3 r g b) = V3 x y z
  where
    x = 0.4124564*r + 0.3575761*g + 0.1804375*b
    y = 0.2126729*r + 0.7151522*g + 0.0721750*b
    z = 0.0193339*r + 0.1191920*g + 0.9503041*b

-- | Convert CIE XYZ to linear RGB (D65 white point)
xyzToLinear :: V3 Float -> V3 Float
xyzToLinear (V3 x y z) = V3 r g b
  where
    r =  3.2404542*x - 1.5371385*y - 0.4985314*z
    g = -0.9692660*x + 1.8760108*y + 0.0415560*z
    b =  0.0556434*x - 0.2040259*y + 1.0572252*z

-- | Lab transfer function
labF :: Float -> Float
labF t
  | t > delta3 = t ** (1/3)
  | otherwise  = t / (3 * delta2) + 4/29
  where
    delta = 6/29
    delta2 = delta * delta
    delta3 = delta2 * delta

-- | Inverse Lab transfer function
labFInv :: Float -> Float
labFInv t
  | t > delta = t * t * t
  | otherwise = 3 * delta2 * (t - 4/29)
  where
    delta = 6/29
    delta2 = delta * delta

-- | Convert CIE XYZ to CIE Lab (D65 white point)
xyzToLab :: V3 Float -> V3 Float
xyzToLab (V3 x y z) = V3 lStar aStar bStar
  where
    -- D65 reference white
    xn = 0.95047
    yn = 1.0
    zn = 1.08883
    fx = labF (x / xn)
    fy = labF (y / yn)
    fz = labF (z / zn)
    lStar = 116 * fy - 16
    aStar = 500 * (fx - fy)
    bStar = 200 * (fy - fz)

-- | Convert CIE Lab to CIE XYZ (D65 white point)
labToXYZ :: V3 Float -> V3 Float
labToXYZ (V3 lStar aStar bStar) = V3 x y z
  where
    -- D65 reference white
    xn = 0.95047
    yn = 1.0
    zn = 1.08883
    fy = (lStar + 16) / 116
    fx = aStar / 500 + fy
    fz = fy - bStar / 200
    x = xn * labFInv fx
    y = yn * labFInv fy
    z = zn * labFInv fz

-- | Convert sRGB to CIE Lab
srgbToLab :: V3 Float -> V3 Float
srgbToLab srgb = xyzToLab (linearToXYZ (fmap srgbToLinear srgb))

-- | Convert CIE Lab to sRGB
labToSrgb :: V3 Float -> V3 Float
labToSrgb lab = fmap linearToSrgb (xyzToLinear (labToXYZ lab))

--------------------------------------------------------------------------------
-- Quantize Context
--------------------------------------------------------------------------------

-- | Environment for quantize shader
data QuantizeEnv os = QuantizeEnv
  { qePrimitives    :: !(PrimitiveArray Triangles RectVertexArr)
    -- ^ Fullscreen quad primitives
  , qeRasterOptions :: !(Side, ViewPort, DepthRange)
    -- ^ Rasterization options
  , qeSourceTex     :: !( Texture2D os (Format RGBAFloat)
                        , SamplerFilter RGBAFloat
                        , (EdgeMode2, BorderColor RGBAFloat)
                        )
    -- ^ Source texture with sampler settings
  , qePaletteTex    :: !( Texture2D os (Format RGBAFloat)
                        , SamplerFilter RGBAFloat
                        , (EdgeMode2, BorderColor RGBAFloat)
                        )
    -- ^ Palette texture (256x2: sRGB in row 0, Lab in row 1)
  , qeTargetImg     :: !(Image (Format RGBAFloat))
    -- ^ Target color image
  , qeDitherStrength :: !Float
    -- ^ Dithering strength (0 = off, 1 = full)
  , qeViewportSize  :: !(V2 Int)
    -- ^ Viewport size for computing pixel coordinates
  }

-- | Context with compiled shader and pre-computed palette texture
data QuantizeContext os = QuantizeContext
  { qcConfig      :: !QuantizeConfig
    -- ^ Configuration
  , qcPaletteTex  :: !(Texture2D os (Format RGBAFloat))
    -- ^ Palette texture (256x2: row 0 = sRGB, row 1 = Lab)
  , qcShader      :: !(CompiledShader os (QuantizeEnv os))
    -- ^ Compiled quantization shader
  , qcQuadBuffs   :: !(RectBuffers os)
    -- ^ Fullscreen quad vertex buffers
  }

--------------------------------------------------------------------------------
-- GPU-side Color Space Conversion (in shader)
--------------------------------------------------------------------------------

-- | GPU-side sRGB to linear conversion
srgbToLinearF :: FFloat -> FFloat
srgbToLinearF c = ifThenElse' (c <=* 0.04045)
  (c / 12.92)
  (((c + 0.055) / 1.055) ** 2.4)

-- | GPU-side linear to sRGB conversion (gamma encode)
linearToSrgbF :: FFloat -> FFloat
linearToSrgbF c = ifThenElse' (c <=* 0.0031308)
  (c * 12.92)
  (1.055 * (c ** 0.4166667) - 0.055)  -- 1/2.4 = 0.4166667

-- | GPU-side linear RGB to XYZ
linearToXYZF :: V3 FFloat -> V3 FFloat
linearToXYZF (V3 r g b) = V3 x y z
  where
    x = 0.4124564*r + 0.3575761*g + 0.1804375*b
    y = 0.2126729*r + 0.7151522*g + 0.0721750*b
    z = 0.0193339*r + 0.1191920*g + 0.9503041*b

-- | GPU-side Lab transfer function
labFF :: FFloat -> FFloat
labFF t = ifThenElse' (t >* delta3)
  (t ** 0.3333333)  -- Use explicit value instead of 1/3
  (t / (3 * delta2) + 0.1379310345)  -- 4/29 as decimal
  where
    delta = 0.20689655 :: FFloat   -- 6/29 as decimal
    delta2 = delta * delta         -- ~0.0428
    delta3 = delta2 * delta        -- ~0.00886

-- | GPU-side XYZ to Lab conversion
xyzToLabF :: V3 FFloat -> V3 FFloat
xyzToLabF (V3 x y z) = V3 lStar aStar bStar
  where
    -- D65 reference white
    xn = 0.95047 :: FFloat
    yn = 1.0 :: FFloat
    zn = 1.08883 :: FFloat
    fx = labFF (x / xn)
    fy = labFF (y / yn)
    fz = labFF (z / zn)
    lStar = 116 * fy - 16
    aStar = 500 * (fx - fy)
    bStar = 200 * (fy - fz)

-- | GPU-side sRGB to Lab conversion
srgbToLabF :: V3 FFloat -> V3 FFloat
srgbToLabF (V3 r g b) = xyzToLabF (linearToXYZF linear)
  where
    linear = V3 (srgbToLinearF r) (srgbToLinearF g) (srgbToLinearF b)

-- | GPU-side linear RGB to Lab conversion (for HDR/linear input from renderer)
-- First converts linear to sRGB (gamma encode), then to Lab.
-- This ensures Lab values match the palette (which is defined in sRGB).
-- Clamps input to [0,1] to handle HDR values.
linearToLabF :: V3 FFloat -> V3 FFloat
linearToLabF (V3 r g b) = srgbToLabF srgb
  where
    -- Clamp to [0,1] for proper conversion of HDR values
    clampF x = maxB 0 (minB x 1)
    -- Convert linear to sRGB (gamma encode) so Lab matches palette
    srgb = V3 (linearToSrgbF (clampF r))
              (linearToSrgbF (clampF g))
              (linearToSrgbF (clampF b))

-- | Squared Euclidean distance in Lab space
labDistanceSq :: V3 FFloat -> V3 FFloat -> FFloat
labDistanceSq (V3 l1 a1 b1) (V3 l2 a2 b2) =
  (l1-l2)*(l1-l2) + (a1-a2)*(a1-a2) + (b1-b2)*(b1-b2)

--------------------------------------------------------------------------------
-- Palette Sampling
--------------------------------------------------------------------------------

-- | Sample Lab color from palette texture at given index (0-255)
-- Lab values are stored normalized: L/100, (a+128)/256, (b+128)/256
-- This function decodes them back to original Lab range.
samplePaletteLab :: Sampler2D (Format RGBAFloat) -> FFloat -> V3 FFloat
samplePaletteLab sampler idx =
  let u = (idx + 0.5) / 256  -- Center of texel
      v = 0.75 :: FFloat     -- Row 1 / top (Lab values)
      V4 lNorm aNorm bNorm _ = sample2D sampler SampleAuto Nothing Nothing (V2 u v)
      -- Decode: L = lNorm * 100, a = aNorm * 256 - 128, b = bNorm * 256 - 128
      l = lNorm * 100
      a = aNorm * 256 - 128
      b = bNorm * 256 - 128
  in V3 l a b

-- | Sample sRGB color from palette texture at given index (0-255)
samplePaletteSrgb :: Sampler2D (Format RGBAFloat) -> FFloat -> V3 FFloat
samplePaletteSrgb sampler idx =
  let u = (idx + 0.5) / 256  -- Center of texel
      v = 0.25 :: FFloat     -- Row 0 / bottom (sRGB values)
      V4 r g b _ = sample2D sampler SampleAuto Nothing Nothing (V2 u v)
  in V3 r g b

--------------------------------------------------------------------------------
-- Ordered Dithering (Bayer Matrix)
--------------------------------------------------------------------------------

-- | Floating-point modulo for GPU shaders: a - floor(a / b) * b
fmod :: FFloat -> FFloat -> FFloat
fmod a b = a - floor' (a / b) * b

-- | Compute 4x4 Bayer matrix threshold for ordered dithering.
-- Returns a value in [-0.5, 0.5] based on pixel position.
-- The Bayer pattern creates a regular dithering pattern that looks good for pixel art.
bayerThreshold :: V2 FFloat -> FFloat
bayerThreshold (V2 x y) =
  let -- Get position within 4x4 tile
      px = fmod x 4
      py = fmod y 4

      -- 4x4 Bayer matrix (normalized to [0, 1])
      -- We compute it procedurally to avoid texture lookup
      -- The formula is based on bit-interleaving of coordinates
      m2x = fmod px 2
      m2y = fmod py 2
      m4x = floor' (px / 2)
      m4y = floor' (py / 2)

      -- Bayer pattern computation
      -- This creates the pattern:
      --  0  8  2 10
      -- 12  4 14  6
      --  3 11  1  9
      -- 15  7 13  5
      v = m2x + m2y * 2 + m4x * 4 + m4y * 8

      -- Rearrange to get proper Bayer ordering
      b0 = fmod v 2
      b1 = fmod (floor' (v / 2)) 2
      b2 = fmod (floor' (v / 4)) 2
      b3 = fmod (floor' (v / 8)) 2

      bayer = b0 * 8 + b1 * 4 + b2 * 2 + b3

  -- Normalize to [-0.5, 0.5]
  in (bayer / 16) - 0.5

-- | Apply ordered dithering to Lab color.
-- Adds a threshold-based offset to the L channel before quantization.
-- This causes pixels in gradient areas to alternate between palette colors.
applyDithering :: V3 FFloat -> V2 FFloat -> FFloat -> V3 FFloat
applyDithering (V3 l a b) pixelPos strength =
  let threshold = bayerThreshold pixelPos
      -- Apply dithering primarily to luminance (L channel)
      -- Scale by ~16 to spread across typical palette L* differences
      ditherAmount = threshold * strength * 16
  in V3 (l + ditherAmount) a b

-- | Find the palette index with closest Lab color using a GPU loop.
-- Iterates through all 256 palette slots to find the minimum distance.
-- For palettes smaller than 256, the extra slots are padded with the first color.
findClosestPaletteIdx :: Sampler2D (Format RGBAFloat) -> V3 FFloat -> FFloat
findClosestPaletteIdx palSampler pixelLab =
  -- Use GPipe's while loop to iterate through palette colors
  -- State is V3: (current index, best index, best distance squared)
  -- GPipe while signature: (a -> S x Bool) -> (a -> a) -> a -> a
  let -- Condition: continue while index < 256
      condition (V3 i _ _) = i <* 256

      -- Step: check current color, update best if closer, increment index
      step (V3 i bestIdx bestDist) =
        let labColor = samplePaletteLab palSampler i
            dist = labDistanceSq pixelLab labColor
            isBetter = dist <* bestDist
            newBestIdx = ifThenElse' isBetter i bestIdx
            newBestDist = ifThenElse' isBetter dist bestDist
        in V3 (i + 1) newBestIdx newBestDist

      -- Initial state: start at index 0, with very large initial distance
      initialState = V3 0 0 1e10

      -- Run loop and extract best index from final state
      V3 _ bestIdx _ = while condition step initialState
  in bestIdx

--------------------------------------------------------------------------------
-- Quantization Shader
--------------------------------------------------------------------------------

-- | Sample dither strength from palette texture.
-- Stored in alpha channel of first Lab pixel (position 0, row 1).
sampleDitherStrength :: Sampler2D (Format RGBAFloat) -> FFloat
sampleDitherStrength sampler =
  let uv = V2 (0.5 / 256) 0.75  -- First pixel of Lab row
      V4 _ _ _ strength = sample2D sampler SampleAuto Nothing Nothing uv
  in strength

-- | Fragment processor: convert to Lab, apply dithering, find closest, return sRGB
-- This version takes pixel coordinates from RasterizedInfo for accurate dithering.
-- Dither strength is read from the palette texture's alpha channel.
quantizeFragmentWithDither
  :: Sampler2D (Format RGBAFloat)  -- ^ Source texture sampler
  -> Sampler2D (Format RGBAFloat)  -- ^ Palette texture sampler
  -> V2 FFloat                     -- ^ UV coordinates
  -> RasterizedInfo                -- ^ Rasterization info with pixel coordinates
  -> V4 FFloat                     -- ^ Output color (RGBA)
quantizeFragmentWithDither srcSampler palSampler uv info =
  let -- Flip V coordinate to match texture orientation
      V2 u v = uv
      flippedUv = V2 u (1 - v)

      -- Get pixel coordinates from rasterized info for dithering
      V4 fragX fragY _ _ = rasterizedFragCoord info
      pixelPos = V2 fragX fragY

      -- Read dither strength from palette texture
      ditherStrength = sampleDitherStrength palSampler

      -- Sample source pixel (linear RGB from renderer, possibly HDR)
      V4 sr sg sb sa = sample2D srcSampler SampleAuto Nothing Nothing flippedUv

      -- Clamp source to [0,1] for proper conversion
      clampF x = maxB 0 (minB x 1)
      clampedSrc = V3 (clampF sr) (clampF sg) (clampF sb)

      -- Convert linear RGB to Lab (input is already linear from renderer)
      pixelLab = linearToLabF clampedSrc

      -- Apply ordered dithering
      ditheredLab = applyDithering pixelLab pixelPos ditherStrength

      -- Find closest palette color index using Lab distance
      closestIdx = findClosestPaletteIdx palSampler ditheredLab

      -- Get sRGB color from palette
      V3 convergenceR convergenceG convergenceB = samplePaletteSrgb palSampler closestIdx

  -- Output palette sRGB converted to linear for display
  in V4 (srgbToLinearF convergenceR) (srgbToLinearF convergenceG) (srgbToLinearF convergenceB) sa

-- | Quantization shader: samples source, quantizes colors, outputs to image
quantizeShader :: Shader os (QuantizeEnv os) ()
quantizeShader = do
  verts <- toPrimitiveStream qePrimitives

  -- Transform fullscreen quad positions to clip space
  let projectedVerts = projectFullscreen <$> verts

  -- Create samplers
  sourceSampler <- newSampler2D qeSourceTex
  paletteSampler <- newSampler2D qePaletteTex

  -- Rasterize
  fragData <- rasterize qeRasterOptions projectedVerts

  -- Apply quantization to each fragment, using rasterized info for pixel position
  let quantizedFrags = withRasterizedInfo
        (\uv info -> quantizeFragmentWithDither sourceSampler paletteSampler uv info)
        fragData
      colorMask = V4 True True True True

  -- Draw to target image
  draw (const NoBlending) quantizedFrags $ \colorVal ->
    drawColor (\env -> (qeTargetImg env, colorMask, False)) colorVal

-- | Project fullscreen vertex to clip space (same as Blit module)
projectFullscreen :: RectVertex -> (V4 VFloat, V2 VFloat)
projectFullscreen RectVertex{..} =
  let V3 x y z = rectPrimPosition
      clipPos = V4 x y z 1
  in (clipPos, rectPrimUv)

--------------------------------------------------------------------------------
-- Palette Texture Creation
--------------------------------------------------------------------------------

-- | Create palette texture from sRGB colors.
-- Stores sRGB values in row 0, Lab values in row 1.
-- Dither strength is stored in the alpha channel of the first Lab pixel.
-- Supports up to 256 colors; smaller palettes are padded with the first color.
createPaletteTexture :: Palette -> Float -> SpiderCtx os (Texture2D os (Format RGBAFloat))
createPaletteTexture palette ditherStrength = do
  -- Create 256x2 texture (256 colors max, 2 rows)
  tex <- newTexture2D RGBA16F (V2 256 2) 1

  -- Pad palette to exactly 256 colors
  -- Pad with first color (or black if empty) so extra slots don't match garbage
  let firstColor = if V.null palette then V3 0 0 0 else V.head palette
      paddedPalette = V.take 256 $ palette V.++ V.replicate 256 firstColor

  -- Row 1 (y=1, top): Lab colors normalized to [0,1] range
  -- Store as: L*/100, (a*+128)/256, (b*+128)/256
  -- First pixel's alpha stores dither strength
  let labColors = V.map srgbToLab paddedPalette
      labPixels = V.toList $ V.imap (\i (V3 l a b) ->
        let alpha = if i == 0 then ditherStrength else 1
        in V4 (l/100) ((a+128)/256) ((b+128)/256) alpha) labColors
  writeTexture2D tex 0 (V2 0 1) (V2 256 1) labPixels

  -- Row 0 (y=0, bottom): sRGB colors
  let srgbPixels = V.toList $ V.map (\(V3 r g b) -> V4 r g b 1) paddedPalette
  writeTexture2D tex 0 (V2 0 0) (V2 256 1) srgbPixels

  pure tex

--------------------------------------------------------------------------------
-- Context Creation
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

-- | Create new quantization context
newQuantizeContext
  :: QuantizeConfig
  -> SpiderCtx os (QuantizeContext os)
newQuantizeContext config = do
  -- Create palette texture with dither strength
  paletteTex <- createPaletteTexture (qcPalette config) (qcDitherStrength config)

  -- Create fullscreen quad buffers
  quadBuffs <- fullscreenQuadBuffers

  -- Compile shader
  shader <- compileShader quantizeShader

  pure QuantizeContext
    { qcConfig = config
    , qcPaletteTex = paletteTex
    , qcShader = shader
    , qcQuadBuffs = quadBuffs
    }

-- | Update the palette in an existing quantization context.
-- This writes new palette data to the existing texture.
-- Preserves the current dither strength stored in the config.
-- Supports up to 256 colors; smaller palettes are padded with the first color.
updateQuantizePalette
  :: QuantizeContext os
  -> Palette
  -> SpiderCtx os ()
updateQuantizePalette ctx@QuantizeContext{..} palette = do
  -- Pad palette to exactly 256 colors
  let firstColor = if V.null palette then V3 0 0 0 else V.head palette
      paddedPalette = V.take 256 $ palette V.++ V.replicate 256 firstColor
      ditherStrength = qcDitherStrength qcConfig

  -- Row 1 (y=1, top): Lab colors normalized to [0,1] range
  -- First pixel's alpha stores dither strength
  let labColors = V.map srgbToLab paddedPalette
      labPixels = V.toList $ V.imap (\i (V3 l a b) ->
        let alpha = if i == 0 then ditherStrength else 1
        in V4 (l/100) ((a+128)/256) ((b+128)/256) alpha) labColors
  writeTexture2D qcPaletteTex 0 (V2 0 1) (V2 256 1) labPixels

  -- Row 0 (y=0, bottom): sRGB colors
  let srgbPixels = V.toList $ V.map (\(V3 r g b) -> V4 r g b 1) paddedPalette
  writeTexture2D qcPaletteTex 0 (V2 0 0) (V2 256 1) srgbPixels

-- | Update just the dither strength in an existing quantization context.
-- This only writes to the alpha channel of the first Lab pixel.
updateDitherStrength
  :: QuantizeContext os
  -> Float
  -> SpiderCtx os ()
updateDitherStrength QuantizeContext{..} ditherStrength = do
  -- Read the current first Lab pixel and update just the alpha
  let V3 l a b = srgbToLab $ if V.null (qcPalette qcConfig)
                              then V3 0 0 0
                              else V.head (qcPalette qcConfig)
      pixel = V4 (l/100) ((a+128)/256) ((b+128)/256) ditherStrength
  writeTexture2D qcPaletteTex 0 (V2 0 1) (V2 1 1) [pixel]

--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------

-- | Apply color quantization to source texture, writing to target image.
-- This should be called within a 'Render' block.
quantizeToImage
  :: QuantizeContext os
  -> Texture2D os (Format RGBAFloat)  -- ^ Source texture (low-res)
  -> Image (Format RGBAFloat)         -- ^ Target image
  -> V2 Int                           -- ^ Target viewport size
  -> Render os ()
quantizeToImage QuantizeContext{..} sourceTex targetImg viewportSize = do
  prims <- rectBufferArray qcQuadBuffs

  qcShader $ QuantizeEnv
    { qePrimitives = prims
    , qeRasterOptions = (FrontAndBack, ViewPort 0 viewportSize, DepthRange 0 1)
    , qeSourceTex = ( sourceTex
                    , SamplerFilter Nearest Nearest Nearest Nothing
                    , (V2 ClampToEdge ClampToEdge, V4 0 0 0 0)
                    )
    , qePaletteTex = ( qcPaletteTex
                     , SamplerFilter Nearest Nearest Nearest Nothing
                     , (V2 ClampToEdge ClampToEdge, V4 0 0 0 0)
                     )
    , qeTargetImg = targetImg
    , qeDitherStrength = qcDitherStrength qcConfig
    , qeViewportSize = viewportSize
    }
