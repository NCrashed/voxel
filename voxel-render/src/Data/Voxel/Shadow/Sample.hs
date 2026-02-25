{-# LANGUAGE NoImplicitPrelude #-}
-- | Shadow sampling utilities for fragment shaders.
--
-- This module provides functions to sample point light shadow cube maps
-- within GPipe fragment shaders. It supports both hard shadows (crisp
-- pixel art style) and soft shadows (PCF filtering).
module Data.Voxel.Shadow.Sample(
    -- * Hard shadow sampling (single light, 6 samplers)
    samplePointShadow
    -- * Slope-scaled shadow sampling
  , samplePointShadowSlope
    -- * Soft shadow sampling (PCF)
  , samplePointShadowPCF
    -- * Multi-light shadow sampling (texture array)
  , samplePointShadowArray
  , sampleMultiPointShadows
    -- * Cube face utilities
  , cubeFaceFromDirection
  , sampleShadowFace
  ) where

import Graphics.GPipe
import Linear
import Prelude hiding ((<*))

import qualified Data.Vector as V

-- | Determine which cube face to sample and compute UV coordinates.
--
-- Given a direction vector from the light to the fragment,
-- returns the face index (0-5) and UV coordinates (0-1) for sampling.
--
-- Face indices: 0=+X, 1=-X, 2=+Y, 3=-Y, 4=+Z, 5=-Z
cubeFaceFromDirection :: V3 FFloat -> (FFloat, V2 FFloat)
cubeFaceFromDirection (V3 x y z) =
  let ax = abs x
      ay = abs y
      az = abs z

      -- Find dominant axis and compute face parameters
      -- Each case returns (faceIndex, majorAxis, s coordinate, t coordinate)
      -- UV coordinates derived from lookAt view matrices in PointLight.hs:
      -- For face F looking in direction D with up U:
      --   right = D × U, true_up = right × D
      --   X_view = dot(dir, right), Y_view = dot(dir, true_up)
      --   NDC = view_xy / (-view_z), UV = (NDC + 1) / 2

      -- +X face: right=(0,0,-1), up=(0,-1,0) → u∝-z, v∝-y
      posXFace = (0, ax, -z, -y)
      -- -X face: right=(0,0,1), up=(0,-1,0) → u∝z, v∝-y
      negXFace = (1, ax, z, -y)
      -- +Y face: right=(1,0,0), up=(0,0,1) → u∝x, v∝z
      posYFace = (2, ay, x, z)
      -- -Y face: right=(1,0,0), up=(0,0,-1) → u∝x, v∝-z
      negYFace = (3, ay, x, -z)
      -- +Z face: right=(1,0,0), up=(0,-1,0) → u∝x, v∝-y
      posZFace = (4, az, x, -y)
      -- -Z face: right=(-1,0,0), up=(0,-1,0) → u∝-x, v∝-y
      negZFace = (5, az, -x, -y)

      -- Select based on dominant axis and sign
      xPosSel = selectTuple4 (x >=* 0) posXFace negXFace
      yPosSel = selectTuple4 (y >=* 0) posYFace negYFace
      zPosSel = selectTuple4 (z >=* 0) posZFace negZFace

      -- Select dominant axis
      xyResult = selectTuple4 (ax >=* ay) xPosSel yPosSel
      (faceIdx, ma, sc, tc) = selectTuple4 (az >=* ax &&* az >=* ay)
                                zPosSel xyResult

      -- Convert to [0,1] UV coordinates
      u = (sc / ma + 1) * 0.5
      v = (tc / ma + 1) * 0.5

  in (faceIdx, V2 u v)

-- | 4-tuple conditional selection for fragment shader
-- Uses element-wise selection with ifThenElse'
selectTuple4 :: FBool -> (FFloat, FFloat, FFloat, FFloat) -> (FFloat, FFloat, FFloat, FFloat) -> (FFloat, FFloat, FFloat, FFloat)
selectTuple4 cond (a1, a2, a3, a4) (b1, b2, b3, b4) =
  ( ifThenElse' cond a1 b1
  , ifThenElse' cond a2 b2
  , ifThenElse' cond a3 b3
  , ifThenElse' cond a4 b4
  )

-- | Sample a specific shadow face based on face index.
--
-- This function samples from one of 6 shadow samplers based on
-- the face index computed from the light direction.
sampleShadowFace
  :: Sampler2D (Format Depth)  -- ^ Face 0 (+X)
  -> Sampler2D (Format Depth)  -- ^ Face 1 (-X)
  -> Sampler2D (Format Depth)  -- ^ Face 2 (+Y)
  -> Sampler2D (Format Depth)  -- ^ Face 3 (-Y)
  -> Sampler2D (Format Depth)  -- ^ Face 4 (+Z)
  -> Sampler2D (Format Depth)  -- ^ Face 5 (-Z)
  -> FFloat                    -- ^ Face index (0-5)
  -> V2 FFloat                 -- ^ UV coordinates
  -> FFloat                    -- ^ Stored depth value
sampleShadowFace s0 s1 s2 s3 s4 s5 faceIdx uv =
  let d0 = sample2D s0 SampleAuto Nothing Nothing uv
      d1 = sample2D s1 SampleAuto Nothing Nothing uv
      d2 = sample2D s2 SampleAuto Nothing Nothing uv
      d3 = sample2D s3 SampleAuto Nothing Nothing uv
      d4 = sample2D s4 SampleAuto Nothing Nothing uv
      d5 = sample2D s5 SampleAuto Nothing Nothing uv

      -- Select based on face index using cascading conditionals
      sel01 = ifThenElse' (faceIdx <* 0.5) d0 d1
      sel23 = ifThenElse' (faceIdx <* 2.5) d2 d3
      sel45 = ifThenElse' (faceIdx <* 4.5) d4 d5
      sel03 = ifThenElse' (faceIdx <* 1.5) sel01 sel23
      result = ifThenElse' (faceIdx <* 3.5) sel03 sel45

  in result

-- | Sample point shadow with hard edges (crisp pixel art style).
--
-- Returns a shadow factor where 0 = fully in shadow, 1 = fully lit.
--
-- This produces crisp, hard-edged shadows matching the voxel aesthetic.
-- Uses slope-scaled bias to handle grazing angles without artifacts.
samplePointShadow
  :: Sampler2D (Format Depth)  -- ^ Face 0 (+X)
  -> Sampler2D (Format Depth)  -- ^ Face 1 (-X)
  -> Sampler2D (Format Depth)  -- ^ Face 2 (+Y)
  -> Sampler2D (Format Depth)  -- ^ Face 3 (-Y)
  -> Sampler2D (Format Depth)  -- ^ Face 4 (+Z)
  -> Sampler2D (Format Depth)  -- ^ Face 5 (-Z)
  -> V3 FFloat                 -- ^ Fragment world position
  -> V3 FFloat                 -- ^ Light position
  -> FFloat                    -- ^ Far plane distance
  -> FFloat                    -- ^ Depth bias (base bias)
  -> FFloat                    -- ^ Shadow factor (0 = shadow, 1 = lit)
samplePointShadow s0 s1 s2 s3 s4 s5 worldPos lightPos far bias =
  let -- Vector from light to fragment
      fragToLight = worldPos - lightPos
      dist = norm fragToLight

      -- Get cube face and UV from direction
      (faceIdx, uv) = cubeFaceFromDirection fragToLight

      -- Normalize distance to [0,1] range for depth comparison
      currentDepth = dist / far

      -- Sample stored depth from shadow map
      storedDepth = sampleShadowFace s0 s1 s2 s3 s4 s5 faceIdx uv

      -- Compare with bias to avoid shadow acne
      -- If current depth (with bias) is less than stored depth, fragment is lit
  in ifThenElse' (currentDepth - bias <* storedDepth) 1.0 0.0

-- | Sample point shadow with slope-scaled bias.
--
-- This version takes the surface normal and computes a bias that
-- increases at grazing angles to prevent shadow acne while minimizing
-- peter-panning (shadow offset) at perpendicular angles.
samplePointShadowSlope
  :: Sampler2D (Format Depth)  -- ^ Face 0 (+X)
  -> Sampler2D (Format Depth)  -- ^ Face 1 (-X)
  -> Sampler2D (Format Depth)  -- ^ Face 2 (+Y)
  -> Sampler2D (Format Depth)  -- ^ Face 3 (-Y)
  -> Sampler2D (Format Depth)  -- ^ Face 4 (+Z)
  -> Sampler2D (Format Depth)  -- ^ Face 5 (-Z)
  -> V3 FFloat                 -- ^ Fragment world position
  -> V3 FFloat                 -- ^ Light position
  -> V3 FFloat                 -- ^ Surface normal (normalized)
  -> FFloat                    -- ^ Far plane distance
  -> FFloat                    -- ^ Min bias (used at perpendicular angles)
  -> FFloat                    -- ^ Max bias (used at grazing angles)
  -> FFloat                    -- ^ Shadow factor (0 = shadow, 1 = lit)
samplePointShadowSlope s0 s1 s2 s3 s4 s5 worldPos lightPos normal far minBias maxBias =
  let -- Vector from fragment to light
      toLight = lightPos - worldPos
      dist = norm toLight
      lightDir = toLight ^/ dist

      -- Compute angle factor: 0 at perpendicular (normal·lightDir = 1),
      -- approaches 1 at grazing angles (normal·lightDir = 0)
      V3 nx ny nz = normal
      V3 lx ly lz = lightDir
      cosAngle = maxB 0.001 (nx * lx + ny * ly + nz * lz)  -- Clamp to avoid div by zero

      -- Slope-scaled bias: increases as surface becomes more parallel to light
      -- Using 1/cosAngle - 1 which is 0 at perpendicular and grows at grazing angles
      slopeFactor = minB 1.0 (1.0 / cosAngle - 1.0)
      scaledBias = minBias + (maxBias - minBias) * slopeFactor

      -- Get cube face and UV from direction (light to fragment)
      fragToLight = worldPos - lightPos
      (faceIdx, uv) = cubeFaceFromDirection fragToLight

      -- Normalize distance to [0,1] range for depth comparison
      currentDepth = dist / far

      -- Sample stored depth from shadow map
      storedDepth = sampleShadowFace s0 s1 s2 s3 s4 s5 faceIdx uv

      -- Compare with scaled bias
  in ifThenElse' (currentDepth - scaledBias <* storedDepth) 1.0 0.0

-- | Sample point shadow with PCF (Percentage Closer Filtering).
--
-- Returns a shadow factor where 0 = fully in shadow, 1 = fully lit.
--
-- This produces softer shadow edges using 4-tap PCF filtering.
-- The softness parameter controls the spread of the filter samples.
samplePointShadowPCF
  :: Sampler2D (Format Depth)  -- ^ Face 0 (+X)
  -> Sampler2D (Format Depth)  -- ^ Face 1 (-X)
  -> Sampler2D (Format Depth)  -- ^ Face 2 (+Y)
  -> Sampler2D (Format Depth)  -- ^ Face 3 (-Y)
  -> Sampler2D (Format Depth)  -- ^ Face 4 (+Z)
  -> Sampler2D (Format Depth)  -- ^ Face 5 (-Z)
  -> V3 FFloat                 -- ^ Fragment world position
  -> V3 FFloat                 -- ^ Light position
  -> FFloat                    -- ^ Far plane distance
  -> FFloat                    -- ^ Depth bias
  -> FFloat                    -- ^ Softness (pixel offset, e.g., 0.001)
  -> FFloat                    -- ^ Shadow factor (0 = shadow, 1 = lit)
samplePointShadowPCF s0 s1 s2 s3 s4 s5 worldPos lightPos far bias softness =
  let -- Vector from light to fragment
      fragToLight = worldPos - lightPos
      dist = norm fragToLight

      -- Get cube face and UV from direction
      (faceIdx, uv) = cubeFaceFromDirection fragToLight
      V2 u v = uv

      -- Normalize distance
      currentDepth = dist / far

      -- PCF offsets (2x2 pattern)
      offset = softness
      uv00 = V2 (u - offset) (v - offset)
      uv10 = V2 (u + offset) (v - offset)
      uv01 = V2 (u - offset) (v + offset)
      uv11 = V2 (u + offset) (v + offset)

      -- Sample 4 points
      d00 = sampleShadowFace s0 s1 s2 s3 s4 s5 faceIdx uv00
      d10 = sampleShadowFace s0 s1 s2 s3 s4 s5 faceIdx uv10
      d01 = sampleShadowFace s0 s1 s2 s3 s4 s5 faceIdx uv01
      d11 = sampleShadowFace s0 s1 s2 s3 s4 s5 faceIdx uv11

      -- Compare each sample
      biasedDepth = currentDepth - bias
      lit00 = ifThenElse' (biasedDepth <* d00) 1.0 0.0
      lit10 = ifThenElse' (biasedDepth <* d10) 1.0 0.0
      lit01 = ifThenElse' (biasedDepth <* d01) 1.0 0.0
      lit11 = ifThenElse' (biasedDepth <* d11) 1.0 0.0

      -- Average the results
  in (lit00 + lit10 + lit01 + lit11) * 0.25

--------------------------------------------------------------------------------
-- Multi-light shadow sampling (texture array)
--------------------------------------------------------------------------------

-- | Sample point shadow from a texture array.
--
-- The texture array contains shadow maps for multiple lights.
-- Layer index = lightIndex * 6 + faceIndex
--
-- Returns shadow factor where 0 = fully in shadow, 1 = fully lit.
samplePointShadowArray
  :: Sampler2DArray (Format Depth)  -- ^ Shadow map texture array
  -> FFloat                         -- ^ Light index as float (0-3)
  -> V3 FFloat                      -- ^ Fragment world position
  -> V3 FFloat                      -- ^ Light position
  -> FFloat                         -- ^ Far plane distance
  -> FFloat                         -- ^ Depth bias
  -> FFloat                         -- ^ Shadow factor (0 = shadow, 1 = lit)
samplePointShadowArray shadowArray lightIdxF worldPos lightPos far bias =
  let -- Vector from light to fragment
      fragToLight = worldPos - lightPos
      dist = norm fragToLight

      -- Get cube face and UV from direction
      (faceIdxF, V2 u v) = cubeFaceFromDirection fragToLight

      -- Compute layer index: lightIndex * 6 + faceIndex (all floats)
      -- faceIdxF is 0,1,2,3,4,5 and lightIdxF is 0,1,2,3
      layerF = lightIdxF * 6 + faceIdxF

      -- Sample from texture array
      -- Note: sample2DArray takes V3 with z being the layer (as float)
      storedDepth = sample2DArray shadowArray SampleAuto Nothing (V3 u v layerF)

      -- Normalize distance to [0,1] range for depth comparison
      currentDepth = dist / far

      -- Compare with bias
  in ifThenElse' (currentDepth - bias <* storedDepth) 1.0 0.0

-- | Sample shadows from multiple point lights using texture array.
--
-- Returns a V4 of shadow factors, one for each of up to 4 lights.
-- Lights beyond the active count return 1.0 (fully lit / no shadow).
--
-- This is the main function to use for multi-light shadow sampling.
sampleMultiPointShadows
  :: Sampler2DArray (Format Depth)  -- ^ Shadow map texture array
  -> FFloat                         -- ^ Number of active shadow lights (0-4) as float
  -> V3 FFloat                      -- ^ Fragment world position
  -> V4 (V3 FFloat)                 -- ^ Light positions (4 lights, unused ones can be zero)
  -> FFloat                         -- ^ Far plane distance
  -> FFloat                         -- ^ Depth bias
  -> V4 FFloat                      -- ^ Shadow factors for each light (0 = shadow, 1 = lit)
sampleMultiPointShadows shadowArray numLightsF worldPos lightPositions far bias =
  let V4 pos0 pos1 pos2 pos3 = lightPositions

      -- Sample each light's shadow (light indices as floats: 0, 1, 2, 3)
      shadow0 = samplePointShadowArray shadowArray 0 worldPos pos0 far bias
      shadow1 = samplePointShadowArray shadowArray 1 worldPos pos1 far bias
      shadow2 = samplePointShadowArray shadowArray 2 worldPos pos2 far bias
      shadow3 = samplePointShadowArray shadowArray 3 worldPos pos3 far bias

      -- Mask out inactive lights (return 1.0 = fully lit for inactive)
      s0 = ifThenElse' (numLightsF >* 0.5) shadow0 1.0
      s1 = ifThenElse' (numLightsF >* 1.5) shadow1 1.0
      s2 = ifThenElse' (numLightsF >* 2.5) shadow2 1.0
      s3 = ifThenElse' (numLightsF >* 3.5) shadow3 1.0

  in V4 s0 s1 s2 s3
