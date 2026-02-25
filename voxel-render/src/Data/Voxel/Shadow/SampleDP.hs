{-# LANGUAGE NoImplicitPrelude #-}
-- | Dual Paraboloid shadow sampling utilities for fragment shaders.
--
-- This module provides functions to sample dual paraboloid shadow maps
-- within GPipe fragment shaders. The paraboloid projection maps each
-- hemisphere of the sphere around a point light onto a 2D texture.
--
-- Layer indexing: layerIndex = lightIndex * 2 + hemisphereIndex
--   where hemisphereIndex is 0 for front (z >= 0) and 1 for back (z < 0)
module Data.Voxel.Shadow.SampleDP(
    -- * Paraboloid projection
    paraboloidProject
    -- * Single light sampling
  , sampleDPShadow
    -- * Multi-light sampling (texture array)
  , sampleDPShadowArray
  , sampleMultiDPShadows
  ) where

import Graphics.GPipe
import Linear
import Prelude hiding ((<*))

-- | Project a direction vector onto paraboloid UV coordinates.
--
-- Given a direction from the light to the fragment, computes:
-- - Which hemisphere (0 = front where z >= 0, 1 = back where z < 0)
-- - UV coordinates in [0, 1] range for sampling
--
-- The paraboloid projection formula:
--   uv = dir.xy / (1 + |dir.z|) * 0.5 + 0.5
paraboloidProject :: V3 FFloat -> (FFloat, V2 FFloat)
paraboloidProject dir =
  let V3 dx dy dz = signorm dir  -- Normalize direction

      -- Determine hemisphere: 0 = front (z >= 0), 1 = back (z < 0)
      hemisphere = ifThenElse' (dz >=* 0) 0 1

      -- Use absolute z for projection
      absZ = abs dz

      -- Paraboloid projection
      denom = 1 + absZ
      u = dx / denom * 0.5 + 0.5
      v = dy / denom * 0.5 + 0.5

  in (hemisphere, V2 u v)

-- | Sample dual paraboloid shadow for a single light.
--
-- Returns shadow factor where 0 = fully in shadow, 1 = fully lit.
sampleDPShadow
  :: Sampler2D (Format Depth)  -- ^ Front hemisphere sampler
  -> Sampler2D (Format Depth)  -- ^ Back hemisphere sampler
  -> V3 FFloat                 -- ^ Fragment world position
  -> V3 FFloat                 -- ^ Light position
  -> FFloat                    -- ^ Far plane distance
  -> FFloat                    -- ^ Depth bias
  -> FFloat                    -- ^ Shadow factor (0 = shadow, 1 = lit)
sampleDPShadow frontSamp backSamp worldPos lightPos far bias =
  let -- Direction from light to fragment
      fragToLight = worldPos - lightPos
      dist = norm fragToLight

      -- Get hemisphere and UV from direction
      (hemisphere, uv) = paraboloidProject fragToLight

      -- Sample from appropriate hemisphere
      frontDepth = sample2D frontSamp SampleAuto Nothing Nothing uv
      backDepth = sample2D backSamp SampleAuto Nothing Nothing uv
      storedDepth = ifThenElse' (hemisphere <* 0.5) frontDepth backDepth

      -- Normalize distance for comparison
      currentDepth = dist / far

      -- Compare with bias
  in ifThenElse' (currentDepth - bias <* storedDepth) 1.0 0.0

-- | Sample dual paraboloid shadow from a texture array.
--
-- The texture array contains shadow maps for multiple lights.
-- Layer index = lightIndex * 2 + hemisphereIndex
--
-- Returns shadow factor where 0 = fully in shadow, 1 = fully lit.
sampleDPShadowArray
  :: Sampler2DArray (Format Depth)  -- ^ Shadow map texture array
  -> FFloat                         -- ^ Light index (0-3)
  -> V3 FFloat                      -- ^ Fragment world position
  -> V3 FFloat                      -- ^ Light position
  -> FFloat                         -- ^ Far plane distance
  -> FFloat                         -- ^ Depth bias
  -> FFloat                         -- ^ Shadow factor (0 = shadow, 1 = lit)
sampleDPShadowArray shadowArray lightIdx worldPos lightPos far bias =
  let -- Direction from light to fragment
      fragToLight = worldPos - lightPos
      dist = norm fragToLight

      -- Get hemisphere and UV from direction
      (hemisphere, V2 u v) = paraboloidProject fragToLight

      -- Compute layer index: lightIndex * 2 + hemisphere
      layerF = lightIdx * 2 + hemisphere

      -- Sample from texture array
      storedDepth = sample2DArray shadowArray SampleAuto Nothing (V3 u v layerF)

      -- Normalize distance for comparison
      currentDepth = dist / far

      -- Compare with bias
  in ifThenElse' (currentDepth - bias <* storedDepth) 1.0 0.0

-- | Sample shadows from multiple point lights using dual paraboloid texture array.
--
-- Returns a V4 of shadow factors, one for each of up to 4 lights.
-- Lights beyond the active count return 1.0 (fully lit / no shadow).
sampleMultiDPShadows
  :: Sampler2DArray (Format Depth)  -- ^ Shadow map texture array
  -> FFloat                         -- ^ Number of active shadow lights (0-4)
  -> V3 FFloat                      -- ^ Fragment world position
  -> V4 (V3 FFloat)                 -- ^ Light positions (4 lights)
  -> FFloat                         -- ^ Far plane distance
  -> FFloat                         -- ^ Depth bias
  -> V4 FFloat                      -- ^ Shadow factors for each light
sampleMultiDPShadows shadowArray numLightsF worldPos lightPositions far bias =
  let V4 pos0 pos1 pos2 pos3 = lightPositions

      -- Sample each light's shadow
      shadow0 = sampleDPShadowArray shadowArray 0 worldPos pos0 far bias
      shadow1 = sampleDPShadowArray shadowArray 1 worldPos pos1 far bias
      shadow2 = sampleDPShadowArray shadowArray 2 worldPos pos2 far bias
      shadow3 = sampleDPShadowArray shadowArray 3 worldPos pos3 far bias

      -- Mask out inactive lights (return 1.0 = fully lit for inactive)
      s0 = ifThenElse' (numLightsF >* 0.5) shadow0 1.0
      s1 = ifThenElse' (numLightsF >* 1.5) shadow1 1.0
      s2 = ifThenElse' (numLightsF >* 2.5) shadow2 1.0
      s3 = ifThenElse' (numLightsF >* 3.5) shadow3 1.0

  in V4 s0 s1 s2 s3
