{-# LANGUAGE NoImplicitPrelude #-}
-- | Dual Paraboloid Shadow Mapping for point lights.
--
-- This module provides an efficient shadow mapping technique that uses
-- only 2 textures per light (front and back hemispheres) instead of 6
-- for cube maps. This allows supporting 4 shadow-casting lights with
-- only 8 texture layers.
--
-- The paraboloid projection warps each hemisphere onto a 2D texture:
--   uv = dir.xy / (1 + |dir.z|) * 0.5 + 0.5
--
-- Trade-offs vs cube maps:
--   + 3x fewer textures per light
--   + Simpler face selection (just z sign)
--   - Slight distortion at hemisphere edges (equator)
--   - Non-linear projection requires more shader math
module Data.Voxel.Shadow.DualParaboloid(
    -- * Configuration
    ShadowConfig(..)
  , defaultShadowConfig
    -- * Shadow map container
  , DualParaboloidShadow(..)
  , maxDPShadowLights
  , newDualParaboloidShadow
    -- * Hemisphere enumeration
  , Hemisphere(..)
  , hemisphereIndex
    -- * Shadow pass shader
  , DPShadowPassEnv(..)
  , dpShadowPassShader
  , renderDPShadowMaps
  , compileDPShadowShaders
  ) where

import Control.Lens ((^.))
import Control.Monad (forM_)
import Data.Int
import Data.Voxel.App
import Data.Voxel.GPipe.Mesh
import Data.Voxel.Transform
import Graphics.GPipe
import Linear
import Prelude hiding ((<*))

import Data.Voxel.Shadow.PointLight (ShadowConfig(..), defaultShadowConfig)

-- | Maximum number of shadow-casting point lights with dual paraboloid
-- Each light needs 2 hemispheres, so 4 lights = 8 texture layers
maxDPShadowLights :: Int
maxDPShadowLights = 4

-- | Hemisphere enumeration for dual paraboloid shadow maps
data Hemisphere = FrontHemi | BackHemi
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Convert hemisphere to index (0 or 1)
hemisphereIndex :: Hemisphere -> Int
hemisphereIndex FrontHemi = 0
hemisphereIndex BackHemi = 1

-- | Uniform buffer for dual paraboloid shadow pass
-- Contains: model matrix, light position, far plane, hemisphere flag
type DPShadowUniform os = Buffer os (Uniform
  ( V4 (B4 Float)  -- Model matrix
  , B3 Float       -- Light position
  , B Float        -- Far plane
  , B Float        -- Hemisphere (0 = front, 1 = back)
  ))

-- | Container for dual paraboloid shadow map resources
data DualParaboloidShadow os = DualParaboloidShadow
  { dpShadowArray   :: !(Texture2DArray os (Format Depth))
    -- ^ Texture array with layers for all lights
    -- Layer index = lightIndex * 2 + hemisphereIndex
  , dpShadowConfig  :: !ShadowConfig
    -- ^ Shadow configuration (shared by all lights)
  , dpShadowUniform :: !(DPShadowUniform os)
    -- ^ Buffer for shadow pass uniforms (reused for each hemisphere)
  }

-- | Create shadow map resources for dual paraboloid shadows
newDualParaboloidShadow :: ShadowConfig -> SpiderCtx os (DualParaboloidShadow os)
newDualParaboloidShadow cfg = do
  let res = shadowResolution cfg
      numLayers = maxDPShadowLights * 2  -- 4 lights × 2 hemispheres = 8 layers

  -- Create texture array for all shadow maps
  shadowArray <- newTexture2DArray Depth32 (V3 res res numLayers) 1

  -- Create uniform buffer for shadow pass
  uniform <- newBuffer 1

  pure DualParaboloidShadow
    { dpShadowArray   = shadowArray
    , dpShadowConfig  = cfg
    , dpShadowUniform = uniform
    }

-- | Environment for dual paraboloid shadow pass rendering
data DPShadowPassEnv os = DPShadowPassEnv
  { dpShadowPrimitives   :: PrimitiveArray Triangles (MeshArray (ArrayOf Int32))
    -- ^ Mesh primitives to render
  , dpShadowRasterOpts   :: (Side, ViewPort, DepthRange)
    -- ^ Rasterization options
  , dpShadowDepthImage   :: Image (Format Depth)
    -- ^ Target depth image (specific layer of array)
  }

-- | Dual paraboloid shadow pass shader
-- Renders depth using paraboloid projection for one hemisphere
dpShadowPassShader
  :: DPShadowUniform os
  -> Shader os (DPShadowPassEnv os) ()
dpShadowPassShader uniform = do
  sides <- toPrimitiveStream dpShadowPrimitives
  (modelMat, lightPos, far, hemisphere) <- getUniform (const (uniform, 0))

  -- Transform vertices with paraboloid projection
  let projectedSides = dpShadowProj modelMat lightPos far hemisphere <$> sides

  -- Rasterize
  fragData <- rasterize dpShadowRasterOpts projectedSides

  -- Fragment shader: output linear depth
  let fragWithDepth = dpShadowFrag <$> fragData
      depthOption = DepthOption Less True

  drawDepth (\env -> (NoBlending, dpShadowDepthImage env, depthOption))
            fragWithDepth
            (\_ -> pure ())

-- | Vertex shader: paraboloid projection
-- Computes clip position using paraboloid projection and passes world position
-- for linear depth calculation in fragment shader.
dpShadowProj :: V4 (V4 VFloat)  -- ^ Model matrix
             -> V3 VFloat       -- ^ Light position
             -> VFloat          -- ^ Far plane
             -> VFloat          -- ^ Hemisphere (0=front, 1=back)
             -> MeshVertex VInt
             -> (V4 VFloat, (V3 VFloat, V3 FlatVFloat, FlatVFloat))
             -- ^ (clip position, (world pos, light pos, far))
dpShadowProj modelMat lightPos far hemisphere MeshVertex{..} =
  let V3 px py pz = meshPrimPosition
      -- Transform to world space
      worldPos4 = modelMat !* V4 px py pz 1
      worldPos = worldPos4 ^. _xyz

      -- Direction from light to vertex
      toVert = worldPos - lightPos
      dist = norm toVert

      -- Normalized direction
      dir = toVert ^/ maxB dist 0.0001  -- Avoid div by zero

      V3 dx dy dz = dir

      -- For back hemisphere, flip z
      z' = ifThenElse' (hemisphere >* 0.5) (negate dz) dz

      -- Paraboloid projection: uv = dir.xy / (1 + z)
      -- We output in clip space [-1, 1]
      denom = maxB (1 + z') 0.0001  -- Avoid div by zero at z = -1
      clipX = dx / denom
      clipY = dy / denom

      -- Clip fragments that are behind the paraboloid (original z on wrong side)
      -- For front hemisphere: clip if z < 0
      -- For back hemisphere: clip if z > 0
      isBehind = ifThenElse' (hemisphere >* 0.5)
                   (dz >* 0.001)   -- Back hemisphere: clip positive z
                   (dz <* (-0.001)) -- Front hemisphere: clip negative z

      -- Push behind far plane to clip
      clipZ = ifThenElse' isBehind 2.0 0.0

  in (V4 clipX clipY clipZ 1, (worldPos, fmap Flat lightPos, Flat far))

-- | Fragment shader: compute linear depth from world position
dpShadowFrag :: (V3 FFloat, V3 FFloat, FFloat)  -- ^ (world pos, light pos, far)
             -> ((), FFloat)  -- ^ (dummy, linear depth)
dpShadowFrag (worldPos, lightPos, far) =
  let dist = norm (worldPos - lightPos)
      linearDepth = dist / far
  in ((), linearDepth)

-- | Render shadow maps for multiple lights using dual paraboloid projection
renderDPShadowMaps
  :: DualParaboloidShadow os
     -- ^ Shadow map resources
  -> CompiledShader os (DPShadowPassEnv os)
     -- ^ Compiled shadow pass shader
  -> [V3 Float]
     -- ^ List of light positions (up to maxDPShadowLights)
  -> Transform Float
     -- ^ Model transformation
  -> Render os (PrimitiveArray Triangles (MeshArray (ArrayOf Int32)))
     -- ^ Primitive stream to render
  -> SpiderCtx os ()
renderDPShadowMaps shadow shader lightPositions modelTransform primStream = do
  let cfg = dpShadowConfig shadow
      res = shadowResolution cfg
      far = shadowFar cfg
      modelMat = transformMatrix modelTransform
      -- Limit to max lights
      lights = take maxDPShadowLights lightPositions

  -- Render each light's shadow maps (front and back hemispheres)
  forM_ (zip [0..] lights) $ \(lightIdx, lightPos) -> do
    -- Render each hemisphere
    forM_ [FrontHemi, BackHemi] $ \hemi -> do
      let hemiIdx = hemisphereIndex hemi
          hemiF = fromIntegral hemiIdx :: Float
          layerIdx = lightIdx * 2 + hemiIdx

      -- Write uniforms
      writeBuffer (dpShadowUniform shadow) 0 [(modelMat, lightPos, far, hemiF)]

      -- Render this hemisphere to the appropriate layer
      render $ do
        depthImg <- getTexture2DArrayImage (dpShadowArray shadow) 0 layerIdx
        clearImageDepth depthImg 1.0

        prims <- primStream
        shader $ DPShadowPassEnv
          { dpShadowPrimitives = prims
          , dpShadowRasterOpts = (FrontAndBack, ViewPort 0 (V2 res res), DepthRange 0 1)
          , dpShadowDepthImage = depthImg
          }

-- | Compile dual paraboloid shadow pass shader
compileDPShadowShaders
  :: DualParaboloidShadow os
  -> SpiderCtx os (CompiledShader os (DPShadowPassEnv os))
compileDPShadowShaders shadow =
  compileShader $ dpShadowPassShader (dpShadowUniform shadow)
