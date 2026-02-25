{-# LANGUAGE NoImplicitPrelude #-}
-- | Point light shadow map rendering infrastructure.
--
-- This module provides cube shadow maps for point lights, allowing
-- shadows to be cast in all directions from a point light source.
-- The shadow map consists of 6 depth textures rendered from the
-- light's perspective.
module Data.Voxel.Shadow.PointLight(
    -- * Configuration
    ShadowConfig(..)
  , defaultShadowConfig
    -- * Shadow map container
  , PointLightShadow(..)
  , newPointLightShadow
    -- * Light-space matrices
  , CubeFace(..)
  , allCubeFaces
  , cubeFaceView
  , lightProjection
    -- * Shadow pass shader
  , ShadowPassEnv(..)
  , shadowPassShader
  , renderShadowCube
  , compileShadowShaders
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

import qualified Data.Vector as V

-- | Configuration for shadow map generation
data ShadowConfig = ShadowConfig
  { shadowResolution :: !Int
    -- ^ Resolution of each cube face texture (e.g., 256, 512, 1024)
  , shadowNear       :: !Float
    -- ^ Near clip plane for shadow rendering
  , shadowFar        :: !Float
    -- ^ Far clip plane (typically the light radius)
  , shadowBias       :: !Float
    -- ^ Depth bias to reduce shadow acne
  } deriving (Show, Eq)

-- | Default shadow configuration suitable for most voxel scenes
defaultShadowConfig :: ShadowConfig
defaultShadowConfig = ShadowConfig
  { shadowResolution = 512
  , shadowNear       = 0.1
  , shadowFar        = 100.0
  , shadowBias       = 0.005
  }

-- | Uniform buffer type for shadow pass
-- Contains: (MVP matrix, model matrix, light position, far plane)
type ShadowUniform os = Buffer os (Uniform
  ( V4 (B4 Float)  -- Light view-projection matrix (for clip space)
  , V4 (B4 Float)  -- Model matrix (for world position)
  , B3 Float       -- Light position
  , B Float        -- Far plane
  ))

-- | Container for point light shadow map resources
data PointLightShadow os = PointLightShadow
  { shadowTextures :: !(V.Vector (Texture2D os (Format Depth)))
    -- ^ 6 depth textures for cube faces (PosX, NegX, PosY, NegY, PosZ, NegZ)
  , shadowConfig   :: !ShadowConfig
    -- ^ Shadow configuration
  , shadowUniform  :: !(ShadowUniform os)
    -- ^ Buffer for shadow pass uniforms
  }

-- | Create shadow map resources for a point light
newPointLightShadow :: ShadowConfig -> SpiderCtx os (PointLightShadow os)
newPointLightShadow cfg = do
  let res = shadowResolution cfg
      size = V2 res res

  -- Create 6 depth textures for cube faces (32-bit for precision)
  textures <- V.replicateM 6 $ newTexture2D Depth32 size 1

  -- Create uniform buffer for shadow pass
  uniform <- newBuffer 1

  pure PointLightShadow
    { shadowTextures = textures
    , shadowConfig   = cfg
    , shadowUniform  = uniform
    }

-- | Cube face enumeration for shadow mapping
data CubeFace
  = PosX  -- ^ +X face (right)
  | NegX  -- ^ -X face (left)
  | PosY  -- ^ +Y face (top)
  | NegY  -- ^ -Y face (bottom)
  | PosZ  -- ^ +Z face (front)
  | NegZ  -- ^ -Z face (back)
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | All cube faces in order
allCubeFaces :: [CubeFace]
allCubeFaces = [PosX, NegX, PosY, NegY, PosZ, NegZ]

-- | Convert cube face to index (0-5)
cubeFaceIndex :: CubeFace -> Int
cubeFaceIndex = fromEnum

-- | Compute view matrix for a cube face from light position
--
-- The view matrix looks from the light position in the direction
-- of the cube face, with appropriate up vector to avoid gimbal lock.
cubeFaceView :: V3 Float -> CubeFace -> M44 Float
cubeFaceView pos face = lookAt pos (pos + dir) up
  where
    (dir, up) = case face of
      PosX -> (V3   1    0    0,  V3 0 (-1) 0)
      NegX -> (V3 (-1)   0    0,  V3 0 (-1) 0)
      PosY -> (V3   0    1    0,  V3 0   0  1)
      NegY -> (V3   0  (-1)   0,  V3 0   0 (-1))
      PosZ -> (V3   0    0    1,  V3 0 (-1) 0)
      NegZ -> (V3   0    0  (-1), V3 0 (-1) 0)

-- | Create 90-degree FOV projection matrix for cube face rendering
--
-- Uses 1:1 aspect ratio and 90 degree FOV so each face covers
-- exactly its quadrant of the unit sphere.
lightProjection :: Float -> Float -> M44 Float
lightProjection near far = perspective (pi / 2) 1.0 near far

-- | Environment for shadow pass rendering
data ShadowPassEnv os = ShadowPassEnv
  { shadowPrimitives   :: PrimitiveArray Triangles (MeshArray (ArrayOf Int32))
    -- ^ Mesh primitives to render
  , shadowRasterOpts   :: (Side, ViewPort, DepthRange)
    -- ^ Rasterization options
  , shadowDepthImage   :: Image (Format Depth)
    -- ^ Target depth image for rendering
  }

-- | Depth-only shader for shadow pass rendering
--
-- This shader transforms vertices to light clip space and outputs
-- LINEAR depth (distance from light / far) for proper point light shadows.
shadowPassShader
  :: ShadowUniform os
     -- ^ Shadow pass uniforms (MVP, model, light pos, far)
  -> Shader os (ShadowPassEnv os) ()
shadowPassShader uniform = do
  sides <- toPrimitiveStream shadowPrimitives
  (lightViewProj, modelMat, lightPos, far) <- getUniform (const (uniform, 0))

  -- Transform vertices: clip space for rasterization, world space + light params for depth
  let projectedSides = shadowProj lightViewProj modelMat lightPos far <$> sides

  -- Rasterize - world position and light params are interpolated to fragment shader
  fragData <- rasterize shadowRasterOpts projectedSides

  -- Compute linear depth in fragment shader
  let fragWithDepth = shadowFrag <$> fragData

  -- Output linear depth to depth buffer
  let depthOption = DepthOption Less True
  drawDepth (\env -> (NoBlending, shadowDepthImage env, depthOption))
            fragWithDepth
            (\_ -> pure ())

-- | Vertex shader: transform to clip space and pass world position with light params
shadowProj :: V4 (V4 VFloat)  -- ^ Light view-projection matrix
           -> V4 (V4 VFloat)  -- ^ Model matrix
           -> V3 VFloat       -- ^ Light position
           -> VFloat          -- ^ Far plane
           -> MeshVertex VInt
           -> (V4 VFloat, (V3 VFloat, V3 FlatVFloat, FlatVFloat))
           -- ^ (clip pos, (world pos, flat light pos, flat far))
shadowProj lightViewProj modelMat lightPos far MeshVertex{..} =
  let V3 px py pz = meshPrimPosition
      -- Transform to world space
      worldPos4 = modelMat !* V4 px py pz 1
      worldPos = worldPos4 ^. _xyz
      -- Transform to clip space for rasterization
      clipPos = lightViewProj !* V4 px py pz 1
  in (clipPos, (worldPos, fmap Flat lightPos, Flat far))

-- | Fragment shader: compute linear depth from world position
shadowFrag :: (V3 FFloat, V3 FFloat, FFloat)  -- ^ (world pos, light pos, far)
           -> ((), FFloat)  -- ^ (dummy, linear depth)
shadowFrag (worldPos, lightPos, far) =
  let dist = norm (worldPos - lightPos)
      linearDepth = dist / far
  in ((), linearDepth)

-- | Render all 6 faces of the shadow cube map
--
-- This function renders the scene from the light's perspective
-- into each of the 6 cube face depth textures, storing LINEAR depth.
renderShadowCube
  :: PointLightShadow os
     -- ^ Shadow map resources
  -> CompiledShader os (ShadowPassEnv os)
     -- ^ Compiled shadow pass shader
  -> V3 Float
     -- ^ Light position
  -> Transform Float
     -- ^ Model transformation
  -> Render os (PrimitiveArray Triangles (MeshArray (ArrayOf Int32)))
     -- ^ Primitive stream to render
  -> SpiderCtx os ()
renderShadowCube shadow shader lightPos modelTransform primStream = do
  let cfg = shadowConfig shadow
      res = shadowResolution cfg
      near = shadowNear cfg
      far = shadowFar cfg
      proj = lightProjection near far
      modelMat = transformMatrix modelTransform

  -- Render each face
  forM_ (zip allCubeFaces [0..5]) $ \(face, idx) -> do
    let viewMat = cubeFaceView lightPos face
        -- MVP includes model matrix for clip space transform
        lightViewProj = proj !*! viewMat !*! modelMat

    -- Write uniforms: (MVP, model matrix, light position, far plane)
    writeBuffer (shadowUniform shadow) 0 [(lightViewProj, modelMat, lightPos, far)]

    -- Render this face
    render $ do
      let tex = shadowTextures shadow V.! idx
      -- Get depth image from texture
      depthImg <- getTexture2DImage tex 0
      clearImageDepth depthImg 1.0

      prims <- primStream
      shader $ ShadowPassEnv
        { shadowPrimitives = prims
        , shadowRasterOpts = (FrontAndBack, ViewPort 0 (V2 res res), DepthRange 0 1)
        , shadowDepthImage = depthImg
        }

-- | Compile shadow pass shader
compileShadowShaders
  :: PointLightShadow os
  -> SpiderCtx os (CompiledShader os (ShadowPassEnv os))
compileShadowShaders shadow =
  compileShader $ shadowPassShader (shadowUniform shadow)
