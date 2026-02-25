{-# LANGUAGE NoImplicitPrelude #-}
-- | Phong shader with texture atlas support using 2D texture arrays
module Data.Voxel.Shader.PhongAtlas(
    -- * Basic shader (no shadows)
    PhongAtlasEnv(..)
  , PhongAtlasUniform
  , phongAtlasShader
  , PhongAtlasContext(..)
  , newPhongAtlasContext
  , renderModelAtlas
    -- * Single-light shadow shader (legacy)
  , PhongAtlasEnvShadow(..)
  , PhongAtlasShadowContext(..)
  , newPhongAtlasShadowContext
  , renderModelAtlasShadow
    -- * Multi-light shadow shader (up to 2 shadow-casting lights)
  , PhongAtlasMultiShadowContext(..)
  , newPhongAtlasMultiShadowContext
  , renderModelAtlasMultiShadow
  , maxMultiShadowLights
  , maxSimpleLights
    -- * Scene models
  , SceneModelAtlas
    -- * Lighting configuration
  , DirectionalLight(..)
  , PointLight(..)
  , Lighting(..)
  , MultiLighting(..)
  , noLight
  , ambientOnly
  , directionalOnly
  , pointOnly
    -- * Shadow configuration (re-exported)
  , ShadowConfig(..)
  , defaultShadowConfig
  , maxShadowLights
  ) where

import Control.Lens ((^.))
import Control.Monad (forM_)
import Data.Voxel.App
import Data.Voxel.Camera
import Data.Voxel.GPipe.Mesh
import Data.Voxel.Material
import Data.Voxel.Shadow.PointLight
import Data.Voxel.Shadow.Sample
import Data.Voxel.Texture.Atlas
import Data.Voxel.Transform
import Data.Int
import Graphics.GPipe
import Linear
import Prelude hiding ((<*))

import qualified Data.Vector as V

-- | Shader environment for atlas-based rendering
data PhongAtlasEnv os = PhongAtlasEnv
  { primitives    :: PrimitiveArray Triangles (MeshArray (ArrayOf Int32))
    -- ^ Mesh primitives with material IDs
  , rasterOptions :: (Side, ViewPort, DepthRange)
    -- ^ Rasterization options
  , diffuseAtlas  :: ( Texture2DArray os (Format RGBAFloat)
                     , SamplerFilter RGBAFloat
                     , (EdgeMode2, BorderColor RGBAFloat)
                     )
    -- ^ Diffuse texture array with sampler settings
  , normalAtlas   :: ( Texture2DArray os (Format RGBAFloat)
                     , SamplerFilter RGBAFloat
                     , (EdgeMode2, BorderColor RGBAFloat)
                     )
    -- ^ Normal map texture array (flat normal texture if not provided)
  , materialLookup :: ( Texture2D os (Format RFloat)
                      , SamplerFilter RFloat
                      , (EdgeMode2, BorderColor RFloat)
                      )
    -- ^ Material lookup texture: X=face index (0-5), Y=material ID
    -- Pixel value = texture layer index
  }

-- | Uniform buffer for MVP matrix, model matrix, normal matrix, and lighting
-- Contains matrices and light parameters needed for rendering.
-- Note: Uses 7-tuple which is the max GPipe supports directly.
type PhongAtlasUniform os = Buffer os (Uniform
  ( V4 (B4 Float)  -- MVP matrix (model-view-projection)
  , V4 (B4 Float)  -- Model matrix (for world position computation)
  , V3 (B3 Float)  -- Normal matrix
  , B4 Float       -- Directional light: (direction.xyz, intensity)
  , B4 Float       -- Point light position + ambient: (pos.xyz, ambient)
  , B4 Float       -- Point light color + power: (color.rgb, power)
  , B3 Float       -- Shadow params: (far, bias, unused)
  ))

-- | Directional light configuration
data DirectionalLight = DirectionalLight
  { dirLightDirection :: !(V3 Float)  -- ^ Direction TO the light (will be normalized)
  , dirLightColor     :: !(V3 Float)  -- ^ RGB color of the light (0-1 range)
  , dirLightIntensity :: !Float       -- ^ Light intensity (0 = disabled)
  } deriving (Show, Eq)

-- | Point light configuration
data PointLight = PointLight
  { pointLightPosition :: !(V3 Float)  -- ^ World position of the light
  , pointLightColor    :: !(V3 Float)  -- ^ RGB color of the light (0-1 range)
  , pointLightPower    :: !Float       -- ^ Light power (affects falloff, 0 = disabled)
  } deriving (Show, Eq)

-- | Combined lighting configuration
data Lighting = Lighting
  { lightingAmbient     :: !Float                -- ^ Ambient light intensity (0-1)
  , lightingDirectional :: !(Maybe DirectionalLight)  -- ^ Optional directional light
  , lightingPoint       :: !(Maybe PointLight)        -- ^ Optional point light
  } deriving (Show, Eq)

-- | No lighting (completely dark except ambient)
noLight :: Lighting
noLight = Lighting 0 Nothing Nothing

-- | Ambient light only
ambientOnly :: Float -> Lighting
ambientOnly amb = Lighting amb Nothing Nothing

-- | Directional light with ambient
directionalOnly :: Float -> DirectionalLight -> Lighting
directionalOnly amb dir = Lighting amb (Just dir) Nothing

-- | Point light with ambient
pointOnly :: Float -> PointLight -> Lighting
pointOnly amb pt = Lighting amb Nothing (Just pt)

-- | Multi-light configuration for scenes with multiple lights
-- NOTE: Limited to 2 shadow-casting point lights due to GPipe FragmentFormat constraints
-- Simple lights (no shadows) are unlimited but only first 4 are rendered for performance
data MultiLighting = MultiLighting
  { multiLightingAmbient      :: !Float                   -- ^ Ambient light intensity (0-1)
  , multiLightingDirectional  :: !(Maybe DirectionalLight) -- ^ Optional directional light
  , multiLightingShadowLights :: ![PointLight]            -- ^ Shadow-casting point lights (max 2)
  , multiLightingSimpleLights :: ![PointLight]            -- ^ Simple point lights without shadows (max 4)
  } deriving (Show, Eq)

-- | Maximum number of simple (non-shadow) lights
maxSimpleLights :: Int
maxSimpleLights = 4

-- | Loaded model to GPU to render with material IDs
type SceneModelAtlas os = Render
       os (PrimitiveArray Triangles (MeshArray (ArrayOf Int32)))

-- | Phong shading with texture atlas support
phongAtlasShader
  :: Window os RGBAFloat Depth
  -> PhongAtlasUniform os
  -> Shader os (PhongAtlasEnv os) ()
phongAtlasShader win uniform = do
  sides <- toPrimitiveStream primitives
  (modelViewProj, modelMat, normMat, dirLightPacked, pointLightPosPacked, pointLightColorPow, _shadowParams)
    <- getUniform (const (uniform, 0))
  -- Unpack directional light (xyz = direction, w = intensity)
  let dirLightDir = dirLightPacked ^. _xyz
      dirIntensity = dirLightPacked ^. _w
      dirLightColorInt = V4 1 1 1 dirIntensity  -- White directional light
  -- Unpack point light (position.xyz, ambient in w)
  let pointLightPos = pointLightPosPacked ^. _xyz
      ambient = pointLightPosPacked ^. _w
  -- Pass light params through vertex shader as Flat values
  let projectedSides = proj modelViewProj modelMat normMat
                         dirLightDir dirLightColorInt pointLightPos pointLightColorPow ambient
                         <$> sides

  -- Create samplers
  diffSampler <- newSampler2DArray diffuseAtlas
  normSampler <- newSampler2DArray normalAtlas
  matSampler <- newSampler2D materialLookup

  fragData <- rasterize rasterOptions projectedSides

  -- Fragment shader: sample texture and apply lighting
  let litFrags = sampleAndLight diffSampler normSampler matSampler <$> fragData
      litFragsWithDepth = withRasterizedInfo
          (\a x -> (a, rasterizedFragCoord x ^. _z)) litFrags
      colorOption = ContextColorOption NoBlending (pure True)
      depthOption = DepthOption Less True

  drawWindowColorDepth (const (win, colorOption, depthOption)) litFragsWithDepth

-- | Project vertex to clip space, passing light params as Flat values
-- Uses nested tuples to stay within GPipe's tuple size limits
proj :: V4 (V4 VFloat)      -- ^ Model-view-projection matrix
     -> V4 (V4 VFloat)      -- ^ Model matrix (for world position)
     -> V3 (V3 VFloat)      -- ^ Normal matrix
     -> V3 VFloat           -- ^ Directional light direction
     -> V4 VFloat           -- ^ Directional light color + intensity
     -> V3 VFloat           -- ^ Point light position
     -> V4 VFloat           -- ^ Point light color + power
     -> VFloat              -- ^ Ambient
     -> MeshVertex VInt
     -> (V4 VFloat, ((V3 FlatVFloat, V2 VFloat, VFloat, V3 VFloat),
                     (V3 FlatVFloat, V4 FlatVFloat, V3 FlatVFloat, V4 FlatVFloat, FlatVFloat)))
proj modelViewProj modelMat normMat dirLightDir dirLightColorInt pointLightPos pointLightColorPow ambient MeshVertex{..} =
  let V3 px py pz = meshPrimPosition
      -- Transform local position to world position
      worldPos4 = modelMat !* V4 px py pz 1
      worldPos = worldPos4 ^. _xyz
      clipPos = modelViewProj !* V4 px py pz 1
      worldNormal = normMat !* meshPrimNormal
      matIdFloat = toFloat meshPrimData
      -- Geometry data (worldPos is now actual world position)
      geomData = (fmap Flat worldNormal, meshPrimUv, matIdFloat, worldPos)
      -- Light parameters (all Flat since constant across vertices)
      lightData = ( fmap Flat dirLightDir
                  , fmap Flat dirLightColorInt
                  , fmap Flat pointLightPos
                  , fmap Flat pointLightColorPow
                  , Flat ambient
                  )
  in (clipPos, (geomData, lightData))

-- | Convert sRGB color to linear space (gamma 2.2)
srgbToLinear :: FFloat -> FFloat
srgbToLinear c = c ** 2.2

-- | Convert linear color to sRGB space (gamma 1/2.2)
linearToSrgb :: FFloat -> FFloat
linearToSrgb c = c ** (1.0 / 2.2)

-- | Sample texture and apply Phong lighting with normal mapping
sampleAndLight
  :: Sampler2DArray (Format RGBAFloat)  -- ^ Diffuse texture sampler
  -> Sampler2DArray (Format RGBAFloat)  -- ^ Normal map sampler
  -> Sampler2D (Format RFloat)          -- ^ Material lookup sampler
  -> ((V3 FFloat, V2 FFloat, FFloat, V3 FFloat),
      (V3 FFloat, V4 FFloat, V3 FFloat, V4 FFloat, FFloat))
      -- ^ ((normal, uv, materialId, worldPos), (dirLightDir, dirLightColorInt, pointLightPos, pointLightColorPow, ambient))
  -> V4 FFloat
sampleAndLight diffSampler normSampler matSampler
  ((normal, uv, matIdF, worldPos), (dirLightDir, dirLightColorInt, pointLightPos, pointLightColorPow, ambient)) =
  let -- Determine face direction from normal (0-5)
      faceIdx = normalToFaceIndex normal

      -- Look up texture layer from material lookup texture
      matUV = V2 ((faceIdx + 0.5) / 6.0) ((matIdF + 0.5) / 256.0)
      texLayer = sample2D matSampler SampleAuto Nothing Nothing matUV

      -- Sample from texture array using UV and layer
      V2 u v = uv
      uvLayer = V3 u v texLayer
      diffuseSrgb = sample2DArray diffSampler SampleAuto Nothing uvLayer
      normalSample = sample2DArray normSampler SampleAuto Nothing uvLayer

      -- Convert diffuse texture from sRGB to linear space for lighting
      V4 dr dg db da = diffuseSrgb
      diffuseLinear = V3 (srgbToLinear dr) (srgbToLinear dg) (srgbToLinear db)

      -- Compute TBN matrix for this face
      (tangent, bitangent, _) = computeTBN normal

      -- Apply normal map to get perturbed normal
      shadingNormal = applyNormalMap normal tangent bitangent normalSample
      V3 nx ny nz = shadingNormal

      -- Unpack directional light parameters
      V4 dirR dirG dirB dirIntensity = dirLightColorInt

      -- Compute directional light contribution
      dirLightDirNorm = signorm dirLightDir
      dirNdotL = maxB 0 (nx * (dirLightDirNorm^._x) + ny * (dirLightDirNorm^._y) + nz * (dirLightDirNorm^._z))
      dirContribR = dirNdotL * dirIntensity * dirR
      dirContribG = dirNdotL * dirIntensity * dirG
      dirContribB = dirNdotL * dirIntensity * dirB

      -- Unpack point light parameters
      V4 ptR ptG ptB ptPower = pointLightColorPow

      -- Compute point light contribution
      toLight = pointLightPos - worldPos
      dist = norm toLight
      pointLightDir = signorm toLight
      -- Attenuation: 1 / (1 + power * dist^2)
      attenuation = 1.0 / (1.0 + ptPower * dist * dist)
      ptNdotL = maxB 0 (nx * (pointLightDir^._x) + ny * (pointLightDir^._y) + nz * (pointLightDir^._z))
      ptContribR = ptNdotL * attenuation * ptR
      ptContribG = ptNdotL * attenuation * ptG
      ptContribB = ptNdotL * attenuation * ptB

      -- Combine all lighting (ambient + directional + point) in linear space
      V3 linR linG linB = diffuseLinear
      litR = linR * (ambient + dirContribR + ptContribR)
      litG = linG * (ambient + dirContribG + ptContribG)
      litB = linB * (ambient + dirContribB + ptContribB)

      -- Convert back to sRGB for display
      outR = linearToSrgb litR
      outG = linearToSrgb litG
      outB = linearToSrgb litB

  in V4 outR outG outB da

-- | Convert axis-aligned normal to face index (0-5)
-- Matches Side enum: 0=+X, 1=-X, 2=+Y, 3=-Y, 4=+Z, 5=-Z
normalToFaceIndex :: V3 FFloat -> FFloat
normalToFaceIndex (V3 nx ny nz) =
  ifThenElse' (nx >* 0.5) 0 $
  ifThenElse' (nx <* (-0.5)) 1 $
  ifThenElse' (ny >* 0.5) 2 $
  ifThenElse' (ny <* (-0.5)) 3 $
  ifThenElse' (nz >* 0.5) 4 5

-- | Compute tangent and bitangent for axis-aligned normals
computeTBN :: V3 FFloat -> (V3 FFloat, V3 FFloat, V3 FFloat)
computeTBN normal = (tangent, bitangent, normal)
  where
    absNz = abs (normal ^. _z)
    absNx = abs (normal ^. _x)
    -- For Z-aligned faces, use X as tangent
    -- For X-aligned faces, use Y as tangent
    -- For Y-aligned faces, use X as tangent
    tangent = ifThenElse' (absNz >* 0.5)
                (V3 1 0 0)
                (ifThenElse' (absNx >* 0.5)
                  (V3 0 1 0)
                  (V3 1 0 0))
    bitangent = cross normal tangent

-- | Apply normal map to surface normal using TBN matrix
applyNormalMap :: V3 FFloat       -- ^ Surface normal
               -> V3 FFloat       -- ^ Tangent
               -> V3 FFloat       -- ^ Bitangent
               -> V4 FFloat       -- ^ Normal map sample (RGBA)
               -> V3 FFloat       -- ^ Transformed world normal
applyNormalMap normal tangent bitangent normalSample =
  let -- Unpack from [0,1] to [-1,1]
      tn = (normalSample ^. _xyz) ^* 2 - 1
      -- TBN transform to world space
      worldN = tangent ^* (tn ^. _x) + bitangent ^* (tn ^. _y) + normal ^* (tn ^. _z)
  in signorm worldN

-- | Environment that is required to draw a single frame with atlas textures
data PhongAtlasContext os = PhongAtlasContext
  { phongAtlasWindow    :: !(Window os RGBAFloat Depth)
    -- ^ Target window to render to
  , phongAtlasMatrix    :: !(PhongAtlasUniform os)
    -- ^ Buffer with MVP matrix and lighting params
  , phongAtlasCamera    :: !(Camera Float)
    -- ^ View projection camera
  , phongAtlasCompiled  :: !(CompiledShader os (PhongAtlasEnv os))
    -- ^ Compiled shader to render with
  , phongAtlasTexture   :: !(TextureAtlas os)
    -- ^ Texture atlas for materials
  , phongAtlasMaterials :: !MaterialTable
    -- ^ Material definitions
  , phongAtlasMatLookup :: !(Texture2D os (Format RFloat))
    -- ^ Material lookup texture (face -> texture layer)
  , phongAtlasNormalTex :: !(Texture2DArray os (Format RGBAFloat))
    -- ^ Normal map texture (either from atlas or flat default)
  }

-- | Create new renderer context for atlas-based rendering
newPhongAtlasContext
  :: Window os RGBAFloat Depth
  -> TextureAtlas os    -- ^ Loaded texture atlas
  -> MaterialTable      -- ^ Material definitions
  -> Camera Float       -- ^ Initial view
  -> SpiderCtx os (PhongAtlasContext os)
newPhongAtlasContext win atlas materials camera = do
  matBuffer <- newBuffer 1

  -- Create material lookup texture
  -- Width = 6 (faces), Height = number of materials (min 256 for shader assumption)
  let (texW, texH, pixels) = materialsToPixels materials
      -- Ensure at least 256 height for the shader's UV calculation
      actualH = max 256 texH
  matLookupTex <- newTexture2D R32F (V2 texW actualH) 1
  writeTexture2D matLookupTex 0 0 (V2 texW texH) pixels

  -- Get or create normal texture
  -- If atlas has normal maps, use them; otherwise create a flat normal texture
  normalTex <- case atlasNormal atlas of
    Just tex -> pure tex
    Nothing -> do
      -- Create a 1x1 flat normal texture (RGB = 0.5, 0.5, 1.0 = pointing up in tangent space)
      let flatNormal :: V4 Float
          flatNormal = V4 0.5 0.5 1.0 1.0
          V2 w h = atlasSize atlas
          numLayers = atlasLayers atlas
      tex <- newTexture2DArray RGBA8 (V3 w h numLayers) 1
      -- Fill all layers with flat normal
      let flatPixels = replicate (w * h) flatNormal
      forM_ [0 .. numLayers - 1] $ \layer ->
        writeTexture2DArray tex 0 (V3 0 0 layer) (V3 w h 1) flatPixels
      pure tex

  shader <- compileShader $ phongAtlasShader win matBuffer
  pure PhongAtlasContext
    { phongAtlasWindow    = win
    , phongAtlasMatrix    = matBuffer
    , phongAtlasCamera    = camera
    , phongAtlasCompiled  = shader
    , phongAtlasTexture   = atlas
    , phongAtlasMaterials = materials
    , phongAtlasMatLookup = matLookupTex
    , phongAtlasNormalTex = normalTex
    }

-- | Render a single model using the atlas shader
renderModelAtlas
  :: PhongAtlasContext os
  -> SceneModelAtlas os   -- ^ Loaded scene model with material IDs
  -> Transform Float      -- ^ Model transformation
  -> Lighting             -- ^ Light configuration
  -> SpiderCtx os ()
renderModelAtlas PhongAtlasContext{..} model transform lighting = do
  -- Write this frame's uniform value
  size@(V2 w h) <- getFrameBufferSize phongAtlasWindow
  let newCam = phongAtlasCamera { cameraAspect = fromIntegral w / fromIntegral h }
      modelMat = transformMatrix transform
      viewProjMat = cameraProjMat newCam !*! cameraViewMat newCam !*! modelMat
      normMat = fromQuaternion (transformRotation transform)

      -- Pack directional light: direction.xyz + intensity in .w
      dirLightPacked = case lightingDirectional lighting of
        Just dl -> let V3 dx dy dz = signorm (dirLightDirection dl)
                   in V4 dx dy dz (dirLightIntensity dl)
        Nothing -> V4 0 0 1 0  -- Disabled (pointing up, zero intensity)

      -- Pack point light position + ambient in .w
      ambient = lightingAmbient lighting
      pointLightPosPacked = case lightingPoint lighting of
        Just pl -> let V3 px py pz = pointLightPosition pl
                   in V4 px py pz ambient
        Nothing -> V4 0 0 0 ambient  -- No point light, but keep ambient

      -- Pack point light color + power
      pointLightColorPow = case lightingPoint lighting of
        Just pl -> let V3 r g b = pointLightColor pl
                   in V4 r g b (pointLightPower pl)
        Nothing -> V4 0 0 0 0  -- Disabled

      -- Shadow params (not used in non-shadow shader, but needed for uniform)
      shadowParams = V3 100.0 0.005 0  -- (shadowFar, shadowBias, unused)

  writeBuffer phongAtlasMatrix 0
    [(viewProjMat, modelMat, normMat, dirLightPacked, pointLightPosPacked, pointLightColorPow, shadowParams)]

  -- Render the frame
  render $ do
    clearWindowColor phongAtlasWindow 0  -- Black
    clearWindowDepth phongAtlasWindow 1  -- Far plane
    prims <- model

    phongAtlasCompiled $ PhongAtlasEnv
      { primitives = prims
      , rasterOptions = (FrontAndBack, ViewPort 0 size, DepthRange 0 1)
      , diffuseAtlas = ( atlasDiffuse phongAtlasTexture
                       , SamplerFilter Nearest Nearest Nearest Nothing
                       , (V2 Repeat Repeat, V4 0 0 0 0)
                       )
      , normalAtlas = ( phongAtlasNormalTex
                      , SamplerFilter Nearest Nearest Nearest Nothing
                      , (V2 Repeat Repeat, V4 0.5 0.5 1.0 1.0)
                      )
      , materialLookup = ( phongAtlasMatLookup
                         , SamplerFilter Nearest Nearest Nearest Nothing
                         , (V2 ClampToEdge ClampToEdge, 0)
                         )
      }
  swapWindowBuffers phongAtlasWindow

--------------------------------------------------------------------------------
-- Shadow-enabled variants
--------------------------------------------------------------------------------

-- | Shader environment for atlas-based rendering with shadow support
data PhongAtlasEnvShadow os = PhongAtlasEnvShadow
  { shadowEnvPrimitives    :: PrimitiveArray Triangles (MeshArray (ArrayOf Int32))
    -- ^ Mesh primitives with material IDs
  , shadowEnvRasterOptions :: (Side, ViewPort, DepthRange)
    -- ^ Rasterization options
  , shadowDiffuseAtlas  :: ( Texture2DArray os (Format RGBAFloat)
                           , SamplerFilter RGBAFloat
                           , (EdgeMode2, BorderColor RGBAFloat)
                           )
    -- ^ Diffuse texture array with sampler settings
  , shadowNormalAtlas   :: ( Texture2DArray os (Format RGBAFloat)
                           , SamplerFilter RGBAFloat
                           , (EdgeMode2, BorderColor RGBAFloat)
                           )
    -- ^ Normal map texture array
  , shadowMaterialLookup :: ( Texture2D os (Format RFloat)
                            , SamplerFilter RFloat
                            , (EdgeMode2, BorderColor RFloat)
                            )
    -- ^ Material lookup texture
  , shadowFace0          :: ( Texture2D os (Format Depth)
                            , SamplerFilter Depth
                            , (EdgeMode2, BorderColor Depth)
                            )
    -- ^ Shadow cube face +X
  , shadowFace1          :: ( Texture2D os (Format Depth)
                            , SamplerFilter Depth
                            , (EdgeMode2, BorderColor Depth)
                            )
    -- ^ Shadow cube face -X
  , shadowFace2          :: ( Texture2D os (Format Depth)
                            , SamplerFilter Depth
                            , (EdgeMode2, BorderColor Depth)
                            )
    -- ^ Shadow cube face +Y
  , shadowFace3          :: ( Texture2D os (Format Depth)
                            , SamplerFilter Depth
                            , (EdgeMode2, BorderColor Depth)
                            )
    -- ^ Shadow cube face -Y
  , shadowFace4          :: ( Texture2D os (Format Depth)
                            , SamplerFilter Depth
                            , (EdgeMode2, BorderColor Depth)
                            )
    -- ^ Shadow cube face +Z
  , shadowFace5          :: ( Texture2D os (Format Depth)
                            , SamplerFilter Depth
                            , (EdgeMode2, BorderColor Depth)
                            )
    -- ^ Shadow cube face -Z
  }

-- | Context for shadow-enabled atlas rendering
data PhongAtlasShadowContext os = PhongAtlasShadowContext
  { psWindow        :: !(Window os RGBAFloat Depth)
    -- ^ Target window to render to
  , psMatrix        :: !(PhongAtlasUniform os)
    -- ^ Buffer with MVP matrix and lighting params
  , psCamera        :: !(Camera Float)
    -- ^ View projection camera
  , psMainShader    :: !(CompiledShader os (PhongAtlasEnvShadow os))
    -- ^ Compiled main shader with shadow sampling
  , psShadowShader  :: !(CompiledShader os (ShadowPassEnv os))
    -- ^ Compiled shadow pass shader
  , psTexture       :: !(TextureAtlas os)
    -- ^ Texture atlas for materials
  , psMaterials     :: !MaterialTable
    -- ^ Material definitions
  , psMatLookup     :: !(Texture2D os (Format RFloat))
    -- ^ Material lookup texture
  , psNormalTex     :: !(Texture2DArray os (Format RGBAFloat))
    -- ^ Normal map texture
  , psShadow        :: !(PointLightShadow os)
    -- ^ Point light shadow map resources
  }

-- | Phong shading with shadow support
phongAtlasShadowShader
  :: Window os RGBAFloat Depth
  -> PhongAtlasUniform os
  -> Shader os (PhongAtlasEnvShadow os) ()
phongAtlasShadowShader win uniform = do
  sides <- toPrimitiveStream shadowEnvPrimitives
  (modelViewProj, modelMat, normMat, dirLightPacked, pointLightPosPacked, pointLightColorPow, shadowParams)
    <- getUniform (const (uniform, 0))

  -- Unpack directional light (xyz = direction, w = intensity)
  let dirLightDir = dirLightPacked ^. _xyz
      dirIntensity = dirLightPacked ^. _w
      dirLightColorInt = V4 1 1 1 dirIntensity  -- White directional light
  -- Unpack point light (position.xyz, ambient in w)
  let pointLightPos = pointLightPosPacked ^. _xyz
      ambient = pointLightPosPacked ^. _w
  -- Extract shadow params (far, bias, unused)
  let shadowFarU = shadowParams ^. _x
      shadowBiasU = shadowParams ^. _y

  let projectedSides = projShadow modelViewProj modelMat normMat
                         dirLightDir dirLightColorInt pointLightPos pointLightColorPow
                         ambient shadowFarU shadowBiasU
                         <$> sides

  -- Create texture samplers
  diffSampler <- newSampler2DArray shadowDiffuseAtlas
  normSampler <- newSampler2DArray shadowNormalAtlas
  matSampler <- newSampler2D shadowMaterialLookup

  -- Create shadow samplers for each cube face
  shadowSamp0 <- newSampler2D shadowFace0
  shadowSamp1 <- newSampler2D shadowFace1
  shadowSamp2 <- newSampler2D shadowFace2
  shadowSamp3 <- newSampler2D shadowFace3
  shadowSamp4 <- newSampler2D shadowFace4
  shadowSamp5 <- newSampler2D shadowFace5

  fragData <- rasterize shadowEnvRasterOptions projectedSides

  -- Fragment shader with shadow sampling
  let litFrags = sampleAndLightShadow diffSampler normSampler matSampler
                   shadowSamp0 shadowSamp1 shadowSamp2
                   shadowSamp3 shadowSamp4 shadowSamp5
                   <$> fragData
      litFragsWithDepth = withRasterizedInfo
          (\a x -> (a, rasterizedFragCoord x ^. _z)) litFrags
      colorOption = ContextColorOption NoBlending (pure True)
      depthOption = DepthOption Less True

  drawWindowColorDepth (const (win, colorOption, depthOption)) litFragsWithDepth

-- | Project vertex to clip space with shadow parameters
projShadow :: V4 (V4 VFloat)  -- ^ Model-view-projection matrix
           -> V4 (V4 VFloat)  -- ^ Model matrix (for world position)
           -> V3 (V3 VFloat)  -- ^ Normal matrix
           -> V3 VFloat       -- ^ Directional light direction
           -> V4 VFloat       -- ^ Directional light color + intensity
           -> V3 VFloat       -- ^ Point light position
           -> V4 VFloat       -- ^ Point light color + power
           -> VFloat          -- ^ Ambient
           -> VFloat          -- ^ Shadow far plane
           -> VFloat          -- ^ Shadow bias
           -> MeshVertex VInt
           -> (V4 VFloat, ((V3 FlatVFloat, V2 VFloat, VFloat, V3 VFloat),
                           (V3 FlatVFloat, V4 FlatVFloat, V3 FlatVFloat, V4 FlatVFloat, FlatVFloat, FlatVFloat, FlatVFloat)))
projShadow modelViewProj modelMat normMat dirLightDir dirLightColorInt pointLightPos pointLightColorPow
           ambient shadowFar shadowBias MeshVertex{..} =
  let V3 px py pz = meshPrimPosition
      -- Transform local position to world position (same as shadow pass)
      worldPos4 = modelMat !* V4 px py pz 1
      worldPos = worldPos4 ^. _xyz
      clipPos = modelViewProj !* V4 px py pz 1
      worldNormal = normMat !* meshPrimNormal
      matIdFloat = toFloat meshPrimData
      -- Geometry data (worldPos is now actual world position)
      geomData = (fmap Flat worldNormal, meshPrimUv, matIdFloat, worldPos)
      -- Light parameters with shadow params (all Flat since constant across vertices)
      lightData = ( fmap Flat dirLightDir
                  , fmap Flat dirLightColorInt
                  , fmap Flat pointLightPos
                  , fmap Flat pointLightColorPow
                  , Flat ambient
                  , Flat shadowFar
                  , Flat shadowBias
                  )
  in (clipPos, (geomData, lightData))

-- | Sample texture and apply Phong lighting with shadows
sampleAndLightShadow
  :: Sampler2DArray (Format RGBAFloat)  -- ^ Diffuse texture sampler
  -> Sampler2DArray (Format RGBAFloat)  -- ^ Normal map sampler
  -> Sampler2D (Format RFloat)          -- ^ Material lookup sampler
  -> Sampler2D (Format Depth)           -- ^ Shadow face 0 (+X)
  -> Sampler2D (Format Depth)           -- ^ Shadow face 1 (-X)
  -> Sampler2D (Format Depth)           -- ^ Shadow face 2 (+Y)
  -> Sampler2D (Format Depth)           -- ^ Shadow face 3 (-Y)
  -> Sampler2D (Format Depth)           -- ^ Shadow face 4 (+Z)
  -> Sampler2D (Format Depth)           -- ^ Shadow face 5 (-Z)
  -> ((V3 FFloat, V2 FFloat, FFloat, V3 FFloat),
      (V3 FFloat, V4 FFloat, V3 FFloat, V4 FFloat, FFloat, FFloat, FFloat))
  -> V4 FFloat
sampleAndLightShadow diffSampler normSampler matSampler
  shadowSamp0 shadowSamp1 shadowSamp2 shadowSamp3 shadowSamp4 shadowSamp5
  ((normal, uv, matIdF, worldPos), (dirLightDir, dirLightColorInt, pointLightPos, pointLightColorPow, ambient, shadowFar, shadowBias)) =
  let -- Determine face direction from normal (0-5)
      faceIdx = normalToFaceIndex normal

      -- Look up texture layer from material lookup texture
      matUV = V2 ((faceIdx + 0.5) / 6.0) ((matIdF + 0.5) / 256.0)
      texLayer = sample2D matSampler SampleAuto Nothing Nothing matUV

      -- Sample from texture array using UV and layer
      V2 u v = uv
      uvLayer = V3 u v texLayer
      diffuseSrgb = sample2DArray diffSampler SampleAuto Nothing uvLayer
      normalSample = sample2DArray normSampler SampleAuto Nothing uvLayer

      -- Convert diffuse texture from sRGB to linear space for lighting
      V4 dr dg db da = diffuseSrgb
      diffuseLinear = V3 (srgbToLinear dr) (srgbToLinear dg) (srgbToLinear db)

      -- Compute TBN matrix for this face
      (tangent, bitangent, _) = computeTBN normal

      -- Apply normal map to get perturbed normal
      shadingNormal = applyNormalMap normal tangent bitangent normalSample
      V3 nx ny nz = shadingNormal

      -- Unpack directional light parameters
      V4 dirR dirG dirB dirIntensity = dirLightColorInt

      -- Compute directional light contribution (no shadows for directional)
      dirLightDirNorm = signorm dirLightDir
      dirNdotL = maxB 0 (nx * (dirLightDirNorm^._x) + ny * (dirLightDirNorm^._y) + nz * (dirLightDirNorm^._z))
      dirContribR = dirNdotL * dirIntensity * dirR
      dirContribG = dirNdotL * dirIntensity * dirG
      dirContribB = dirNdotL * dirIntensity * dirB

      -- Unpack point light parameters
      V4 ptR ptG ptB ptPower = pointLightColorPow

      -- Compute shadow factor for point light
      -- Use small depth bias for comparison
      shadowFactor = samplePointShadow
                       shadowSamp0 shadowSamp1 shadowSamp2
                       shadowSamp3 shadowSamp4 shadowSamp5
                       worldPos pointLightPos shadowFar shadowBias

      -- Compute point light contribution with shadow
      toLight = pointLightPos - worldPos
      dist = norm toLight
      pointLightDir = signorm toLight
      attenuation = 1.0 / (1.0 + ptPower * dist * dist)
      ptNdotL = maxB 0 (nx * (pointLightDir^._x) + ny * (pointLightDir^._y) + nz * (pointLightDir^._z))
      -- Multiply by shadow factor
      ptContribR = shadowFactor * ptNdotL * attenuation * ptR
      ptContribG = shadowFactor * ptNdotL * attenuation * ptG
      ptContribB = shadowFactor * ptNdotL * attenuation * ptB

      -- Combine all lighting in linear space
      V3 linR linG linB = diffuseLinear
      litR = linR * (ambient + dirContribR + ptContribR)
      litG = linG * (ambient + dirContribG + ptContribG)
      litB = linB * (ambient + dirContribB + ptContribB)

      -- Convert back to sRGB for display
      outR = linearToSrgb litR
      outG = linearToSrgb litG
      outB = linearToSrgb litB

  in V4 outR outG outB da

-- | Create new shadow-enabled renderer context
newPhongAtlasShadowContext
  :: Window os RGBAFloat Depth
  -> TextureAtlas os    -- ^ Loaded texture atlas
  -> MaterialTable      -- ^ Material definitions
  -> Camera Float       -- ^ Initial view
  -> ShadowConfig       -- ^ Shadow configuration
  -> SpiderCtx os (PhongAtlasShadowContext os)
newPhongAtlasShadowContext win atlas materials camera shadowCfg = do
  matBuffer <- newBuffer 1

  -- Create material lookup texture
  let (texW, texH, pixels) = materialsToPixels materials
      actualH = max 256 texH
  matLookupTex <- newTexture2D R32F (V2 texW actualH) 1
  writeTexture2D matLookupTex 0 0 (V2 texW texH) pixels

  -- Get or create normal texture
  normalTex <- case atlasNormal atlas of
    Just tex -> pure tex
    Nothing -> do
      let flatNormal :: V4 Float
          flatNormal = V4 0.5 0.5 1.0 1.0
          V2 w h = atlasSize atlas
          numLayers = atlasLayers atlas
      tex <- newTexture2DArray RGBA8 (V3 w h numLayers) 1
      let flatPixels = replicate (w * h) flatNormal
      forM_ [0 .. numLayers - 1] $ \layer ->
        writeTexture2DArray tex 0 (V3 0 0 layer) (V3 w h 1) flatPixels
      pure tex

  -- Create shadow map resources
  shadow <- newPointLightShadow shadowCfg

  -- Compile shadow pass shader
  shadowShader <- compileShadowShaders shadow

  -- Compile main shader with shadow sampling
  mainShader <- compileShader $ phongAtlasShadowShader win matBuffer

  pure PhongAtlasShadowContext
    { psWindow        = win
    , psMatrix        = matBuffer
    , psCamera        = camera
    , psMainShader    = mainShader
    , psShadowShader  = shadowShader
    , psTexture       = atlas
    , psMaterials     = materials
    , psMatLookup     = matLookupTex
    , psNormalTex     = normalTex
    , psShadow        = shadow
    }

-- | Render a model with point light shadows
renderModelAtlasShadow
  :: PhongAtlasShadowContext os
  -> SceneModelAtlas os   -- ^ Loaded scene model with material IDs
  -> Transform Float      -- ^ Model transformation
  -> Lighting             -- ^ Light configuration
  -> SpiderCtx os ()
renderModelAtlasShadow PhongAtlasShadowContext{..} model transform lighting = do
  -- Step 1: Render shadow map if point light is enabled
  case lightingPoint lighting of
    Just pl -> renderShadowCube psShadow psShadowShader
                 (pointLightPosition pl) transform model
    Nothing -> pure ()

  -- Step 2: Render main scene with shadows
  size@(V2 w h) <- getFrameBufferSize psWindow
  let newCam = psCamera { cameraAspect = fromIntegral w / fromIntegral h }
      modelMat = transformMatrix transform
      viewProjMat = cameraProjMat newCam !*! cameraViewMat newCam !*! modelMat
      normMat = fromQuaternion (transformRotation transform)

      -- Pack directional light: direction.xyz + intensity in .w
      dirLightPacked = case lightingDirectional lighting of
        Just dl -> let V3 dx dy dz = signorm (dirLightDirection dl)
                   in V4 dx dy dz (dirLightIntensity dl)
        Nothing -> V4 0 0 1 0  -- Disabled (pointing up, zero intensity)

      -- Pack point light position + ambient in .w
      ambient = lightingAmbient lighting
      pointLightPosPacked = case lightingPoint lighting of
        Just pl -> let V3 px py pz = pointLightPosition pl
                   in V4 px py pz ambient
        Nothing -> V4 0 0 0 ambient  -- No point light, but keep ambient

      -- Pack point light color + power
      pointLightColorPow = case lightingPoint lighting of
        Just pl -> let V3 r g b = pointLightColor pl
                   in V4 r g b (pointLightPower pl)
        Nothing -> V4 0 0 0 0  -- Disabled

      -- Shadow parameters from config - pack into V3 (far, bias, unused)
      cfg = shadowConfig psShadow
      shadowFarVal = shadowFar cfg
      shadowBiasVal = shadowBias cfg
      shadowParams = V3 shadowFarVal shadowBiasVal 0

      -- Shadow texture settings
      shadowSamplerFilter = SamplerFilter Nearest Nearest Nearest Nothing
      shadowEdgeMode = (V2 ClampToEdge ClampToEdge, 1.0)

  writeBuffer psMatrix 0
    [(viewProjMat, modelMat, normMat, dirLightPacked, pointLightPosPacked, pointLightColorPow, shadowParams)]

  render $ do
    clearWindowColor psWindow 0
    clearWindowDepth psWindow 1
    prims <- model

    let shadowTexs = shadowTextures psShadow

    psMainShader $ PhongAtlasEnvShadow
      { shadowEnvPrimitives = prims
      , shadowEnvRasterOptions = (FrontAndBack, ViewPort 0 size, DepthRange 0 1)
      , shadowDiffuseAtlas = ( atlasDiffuse psTexture
                             , SamplerFilter Nearest Nearest Nearest Nothing
                             , (V2 Repeat Repeat, V4 0 0 0 0)
                             )
      , shadowNormalAtlas = ( psNormalTex
                            , SamplerFilter Nearest Nearest Nearest Nothing
                            , (V2 Repeat Repeat, V4 0.5 0.5 1.0 1.0)
                            )
      , shadowMaterialLookup = ( psMatLookup
                               , SamplerFilter Nearest Nearest Nearest Nothing
                               , (V2 ClampToEdge ClampToEdge, 0)
                               )
      , shadowFace0 = (shadowTexs V.! 0, shadowSamplerFilter, shadowEdgeMode)
      , shadowFace1 = (shadowTexs V.! 1, shadowSamplerFilter, shadowEdgeMode)
      , shadowFace2 = (shadowTexs V.! 2, shadowSamplerFilter, shadowEdgeMode)
      , shadowFace3 = (shadowTexs V.! 3, shadowSamplerFilter, shadowEdgeMode)
      , shadowFace4 = (shadowTexs V.! 4, shadowSamplerFilter, shadowEdgeMode)
      , shadowFace5 = (shadowTexs V.! 5, shadowSamplerFilter, shadowEdgeMode)
      }
  swapWindowBuffers psWindow

--------------------------------------------------------------------------------
-- Multi-light shadow support (up to 4 shadow-casting point lights)
--------------------------------------------------------------------------------

-- | Uniform buffer for multi-light shadow shader
-- Contains: MVP, model matrix, normal matrix, directional light,
-- 2 shadow-casting lights, 4 simple lights, and parameters
-- NOTE: Limited to 2 shadow-casting lights due to GPipe FragmentFormat limitations
type MultiShadowUniform os = Buffer os (Uniform
  ( V4 (B4 Float)  -- MVP matrix
  , V4 (B4 Float)  -- Model matrix
  , V3 (B3 Float)  -- Normal matrix
  , B4 Float       -- Directional light: direction.xyz, intensity
  , B4 Float       -- Shadow light 0: pos.xyz, power
  , B4 Float       -- Shadow light 1: pos.xyz, power
  , ( B4 Float     -- Shadow light 0 color.rgb, unused
    , B4 Float     -- Shadow light 1 color.rgb, unused
    , B4 Float     -- (ambient, numShadowLights, shadowFar, shadowBias)
    , ( B4 Float   -- Simple light 0: pos.xyz, power
      , B4 Float   -- Simple light 1: pos.xyz, power
      , B4 Float   -- Simple light 2: pos.xyz, power
      , B4 Float   -- Simple light 3: pos.xyz, power
      , ( B4 Float -- Simple light 0 color.rgb, numSimpleLights in w
        , B4 Float -- Simple light 1 color.rgb, unused
        , B4 Float -- Simple light 2 color.rgb, unused
        , B4 Float -- Simple light 3 color.rgb, unused
        )
      )
    )
  ))

-- | Shader environment for multi-light shadow rendering
data PhongAtlasEnvMultiShadow os = PhongAtlasEnvMultiShadow
  { multiShadowPrimitives    :: PrimitiveArray Triangles (MeshArray (ArrayOf Int32))
  , multiShadowRasterOptions :: (Side, ViewPort, DepthRange)
  , multiShadowDiffuseAtlas  :: ( Texture2DArray os (Format RGBAFloat)
                                , SamplerFilter RGBAFloat
                                , (EdgeMode2, BorderColor RGBAFloat)
                                )
  , multiShadowNormalAtlas   :: ( Texture2DArray os (Format RGBAFloat)
                                , SamplerFilter RGBAFloat
                                , (EdgeMode2, BorderColor RGBAFloat)
                                )
  , multiShadowMaterialLookup :: ( Texture2D os (Format RFloat)
                                 , SamplerFilter RFloat
                                 , (EdgeMode2, BorderColor RFloat)
                                 )
  , multiShadowDepthArray    :: ( Texture2DArray os (Format Depth)
                                , SamplerFilter Depth
                                , (EdgeMode2, BorderColor Depth)
                                )
    -- ^ Single texture array containing all shadow maps (2 lights × 6 faces = 12 layers)
  }

-- | Context for multi-light shadow rendering
data PhongAtlasMultiShadowContext os = PhongAtlasMultiShadowContext
  { pmsWindow       :: !(Window os RGBAFloat Depth)
  , pmsMatrix       :: !(MultiShadowUniform os)
  , pmsCamera       :: !(Camera Float)
  , pmsMainShader   :: !(CompiledShader os (PhongAtlasEnvMultiShadow os))
  , pmsShadowShader :: !(CompiledShader os (ShadowPassEnvArray os))
  , pmsTexture      :: !(TextureAtlas os)
  , pmsMaterials    :: !MaterialTable
  , pmsMatLookup    :: !(Texture2D os (Format RFloat))
  , pmsNormalTex    :: !(Texture2DArray os (Format RGBAFloat))
  , pmsShadow       :: !(MultiPointLightShadow os)
  }

-- | Multi-light shadow shader (supports up to 2 shadow-casting + 4 simple point lights)
phongAtlasMultiShadowShader
  :: Window os RGBAFloat Depth
  -> MultiShadowUniform os
  -> Shader os (PhongAtlasEnvMultiShadow os) ()
phongAtlasMultiShadowShader win uniform = do
  sides <- toPrimitiveStream multiShadowPrimitives
  (modelViewProj, modelMat, normMat, dirLightPacked, light0Pos, light1Pos,
   (light0Col, light1Col, params,
    (simple0Pos, simple1Pos, simple2Pos, simple3Pos,
     (simple0ColAndCount, simple1Col, simple2Col, simple3Col))))
    <- getUniform (const (uniform, 0))

  -- Unpack parameters
  let dirLightDir = dirLightPacked ^. _xyz
      dirIntensity = dirLightPacked ^. _w
      dirLightColorInt = V4 1 1 1 dirIntensity

  let ambient = params ^. _x
      numShadowLightsF = params ^. _y
      shadowFarU = params ^. _z
      shadowBiasU = params ^. _w

  -- Number of simple lights is packed in w component of first simple light color
  let numSimpleLightsF = simple0ColAndCount ^. _w

  let projectedSides = projMultiShadowWithSimple modelViewProj modelMat normMat
                         dirLightDir dirLightColorInt
                         light0Pos light1Pos light0Col light1Col
                         simple0Pos simple1Pos simple2Pos simple3Pos
                         simple0ColAndCount simple1Col simple2Col simple3Col
                         ambient numShadowLightsF numSimpleLightsF shadowFarU shadowBiasU
                         <$> sides

  -- Create samplers
  diffSampler <- newSampler2DArray multiShadowDiffuseAtlas
  normSampler <- newSampler2DArray multiShadowNormalAtlas
  matSampler <- newSampler2D multiShadowMaterialLookup
  shadowSampler <- newSampler2DArray multiShadowDepthArray

  fragData <- rasterize multiShadowRasterOptions projectedSides

  let litFrags = sampleAndLightWithSimple diffSampler normSampler matSampler shadowSampler
                   <$> fragData
      litFragsWithDepth = withRasterizedInfo
          (\a x -> (a, rasterizedFragCoord x ^. _z)) litFrags
      colorOption = ContextColorOption NoBlending (pure True)
      depthOption = DepthOption Less True

  drawWindowColorDepth (const (win, colorOption, depthOption)) litFragsWithDepth

-- | Vertex shader for 2-light shadows
-- Uses nested pairs to avoid large tuples (GPipe FragmentFormat limitation)
projMultiShadow2
  :: V4 (V4 VFloat) -> V4 (V4 VFloat) -> V3 (V3 VFloat)
  -> V3 VFloat -> V4 VFloat
  -> V4 VFloat -> V4 VFloat    -- Light 0 and 1 positions (xyz + power in w)
  -> V4 VFloat -> V4 VFloat    -- Light 0 and 1 colors (rgb, unused w)
  -> VFloat -> VFloat -> VFloat -> VFloat
  -> MeshVertex VInt
  -> (V4 VFloat, ((V3 FlatVFloat, V2 VFloat, VFloat, V3 VFloat),
                  ((V3 FlatVFloat, V4 FlatVFloat),                     -- dir light
                   ((V4 FlatVFloat, V4 FlatVFloat),                    -- light positions
                    ((V4 FlatVFloat, V4 FlatVFloat),                   -- light colors
                     (FlatVFloat, FlatVFloat, FlatVFloat, FlatVFloat))))))  -- params
projMultiShadow2 modelViewProj modelMat normMat dirLightDir dirLightColorInt
                 light0Pos light1Pos light0Col light1Col
                 ambient numLightsF shadowFar shadowBias MeshVertex{..} =
  let V3 px py pz = meshPrimPosition
      worldPos4 = modelMat !* V4 px py pz 1
      worldPos = worldPos4 ^. _xyz
      clipPos = modelViewProj !* V4 px py pz 1
      worldNormal = normMat !* meshPrimNormal
      matIdFloat = toFloat meshPrimData
      geomData = (fmap Flat worldNormal, meshPrimUv, matIdFloat, worldPos)
      lightData = ( (fmap Flat dirLightDir, fmap Flat dirLightColorInt)
                  , ( (fmap Flat light0Pos, fmap Flat light1Pos)
                    , ( (fmap Flat light0Col, fmap Flat light1Col)
                      , (Flat ambient, Flat numLightsF, Flat shadowFar, Flat shadowBias)
                      )
                    )
                  )
  in (clipPos, (geomData, lightData))

-- | Fragment shader for 2-light shadows
sampleAndLightMultiShadow2
  :: Sampler2DArray (Format RGBAFloat)
  -> Sampler2DArray (Format RGBAFloat)
  -> Sampler2D (Format RFloat)
  -> Sampler2DArray (Format Depth)
  -> ((V3 FFloat, V2 FFloat, FFloat, V3 FFloat),
      ((V3 FFloat, V4 FFloat),                     -- dir light
       ((V4 FFloat, V4 FFloat),                    -- light positions
        ((V4 FFloat, V4 FFloat),                   -- light colors
         (FFloat, FFloat, FFloat, FFloat)))))      -- params
  -> V4 FFloat
sampleAndLightMultiShadow2 diffSampler normSampler matSampler shadowSampler
  ((normal, uv, matIdF, worldPos),
   ((dirLightDir, dirLightColorInt),
    ((light0Pos, light1Pos),
     ((light0Col, light1Col),
      (ambient, numLightsF, shadowFar, shadowBias))))) =
  let -- Determine face direction from normal
      faceIdx = normalToFaceIndex normal

      -- Look up texture layer
      matUV = V2 ((faceIdx + 0.5) / 6.0) ((matIdF + 0.5) / 256.0)
      texLayer = sample2D matSampler SampleAuto Nothing Nothing matUV

      -- Sample textures
      V2 u v = uv
      uvLayer = V3 u v texLayer
      diffuseSrgb = sample2DArray diffSampler SampleAuto Nothing uvLayer
      normalSample = sample2DArray normSampler SampleAuto Nothing uvLayer

      -- Convert to linear
      V4 dr dg db da = diffuseSrgb
      diffuseLinear = V3 (srgbToLinear dr) (srgbToLinear dg) (srgbToLinear db)

      -- Normal mapping
      (tangent, bitangent, _) = computeTBN normal
      shadingNormal = applyNormalMap normal tangent bitangent normalSample
      V3 nx ny nz = shadingNormal

      -- Directional light
      V4 dirR dirG dirB dirIntensity = dirLightColorInt
      dirLightDirNorm = signorm dirLightDir
      dirNdotL = maxB 0 (nx * (dirLightDirNorm^._x) + ny * (dirLightDirNorm^._y) + nz * (dirLightDirNorm^._z))
      dirContribR = dirNdotL * dirIntensity * dirR
      dirContribG = dirNdotL * dirIntensity * dirG
      dirContribB = dirNdotL * dirIntensity * dirB

      -- Extract position.xyz and power from each light
      lightPos0 = light0Pos ^. _xyz
      lightPos1 = light1Pos ^. _xyz
      power0 = light0Pos ^. _w
      power1 = light1Pos ^. _w

      -- Sample shadows for both lights
      shadow0 = samplePointShadowArray shadowSampler 0 worldPos lightPos0 shadowFar shadowBias
      shadow1 = samplePointShadowArray shadowSampler 1 worldPos lightPos1 shadowFar shadowBias
      -- Only apply shadow if light is active
      sf0 = ifThenElse' (numLightsF >* 0.5) shadow0 1.0
      sf1 = ifThenElse' (numLightsF >* 1.5) shadow1 1.0

      -- Compute point light contributions
      ptContrib0 = computePointLightContrib shadingNormal worldPos lightPos0 (light0Col ^. _xyz) power0 sf0
      ptContrib1 = computePointLightContrib shadingNormal worldPos lightPos1 (light1Col ^. _xyz) power1 sf1

      -- Combine point light contributions
      totalPtContrib = ptContrib0 + ptContrib1

      -- Final lighting
      V3 linR linG linB = diffuseLinear
      V3 ptR ptG ptB = totalPtContrib
      litR = linR * (ambient + dirContribR + ptR)
      litG = linG * (ambient + dirContribG + ptG)
      litB = linB * (ambient + dirContribB + ptB)

      -- Convert to sRGB
      outR = linearToSrgb litR
      outG = linearToSrgb litG
      outB = linearToSrgb litB

  in V4 outR outG outB da

-- | Vertex shader for 2 shadow lights + 4 simple lights
-- Uses deeply nested pairs to satisfy GPipe FragmentFormat constraints
projMultiShadowWithSimple
  :: V4 (V4 VFloat) -> V4 (V4 VFloat) -> V3 (V3 VFloat)
  -> V3 VFloat -> V4 VFloat                              -- dir light
  -> V4 VFloat -> V4 VFloat -> V4 VFloat -> V4 VFloat    -- shadow light pos/col
  -> V4 VFloat -> V4 VFloat -> V4 VFloat -> V4 VFloat    -- simple light positions
  -> V4 VFloat -> V4 VFloat -> V4 VFloat -> V4 VFloat    -- simple light colors
  -> VFloat -> VFloat -> VFloat -> VFloat -> VFloat      -- params
  -> MeshVertex VInt
  -> (V4 VFloat,
      ((V3 FlatVFloat, V2 VFloat, VFloat, V3 VFloat),    -- geom data
       ((V3 FlatVFloat, V4 FlatVFloat),                  -- dir light
        ((V4 FlatVFloat, V4 FlatVFloat),                 -- shadow positions
         ((V4 FlatVFloat, V4 FlatVFloat),                -- shadow colors
          ((FlatVFloat, FlatVFloat, FlatVFloat, FlatVFloat),  -- params
           ((V4 FlatVFloat, V4 FlatVFloat),              -- simple pos 0,1
            ((V4 FlatVFloat, V4 FlatVFloat),             -- simple pos 2,3
             ((V4 FlatVFloat, V4 FlatVFloat),            -- simple col 0,1
              (V4 FlatVFloat, V4 FlatVFloat))))))))))   -- simple col 2,3
projMultiShadowWithSimple modelViewProj modelMat normMat
                          dirLightDir dirLightColorInt
                          shadowPos0 shadowPos1 shadowCol0 shadowCol1
                          simplePos0 simplePos1 simplePos2 simplePos3
                          simpleCol0 simpleCol1 simpleCol2 simpleCol3
                          ambient numShadowF numSimpleF shadowFar shadowBias
                          MeshVertex{..} =
  let V3 px py pz = meshPrimPosition
      worldPos4 = modelMat !* V4 px py pz 1
      worldPos = worldPos4 ^. _xyz
      clipPos = modelViewProj !* V4 px py pz 1
      worldNormal = normMat !* meshPrimNormal
      matIdFloat = toFloat meshPrimData
      geomData = (fmap Flat worldNormal, meshPrimUv, matIdFloat, worldPos)
      lightData =
        ( (fmap Flat dirLightDir, fmap Flat dirLightColorInt)
        , ( (fmap Flat shadowPos0, fmap Flat shadowPos1)
          , ( (fmap Flat shadowCol0, fmap Flat shadowCol1)
            , ( (Flat ambient, Flat numShadowF, Flat shadowFar, Flat shadowBias)
              , ( (fmap Flat simplePos0, fmap Flat simplePos1)
                , ( (fmap Flat simplePos2, fmap Flat simplePos3)
                  , ( (fmap Flat simpleCol0, fmap Flat simpleCol1)
                    , (fmap Flat simpleCol2, fmap Flat simpleCol3)
                    )
                  )
                )
              )
            )
          )
        )
  in (clipPos, (geomData, lightData))

-- | Fragment shader for 2 shadow lights + 4 simple lights
sampleAndLightWithSimple
  :: Sampler2DArray (Format RGBAFloat)
  -> Sampler2DArray (Format RGBAFloat)
  -> Sampler2D (Format RFloat)
  -> Sampler2DArray (Format Depth)
  -> ((V3 FFloat, V2 FFloat, FFloat, V3 FFloat),
      ((V3 FFloat, V4 FFloat),
       ((V4 FFloat, V4 FFloat),
        ((V4 FFloat, V4 FFloat),
         ((FFloat, FFloat, FFloat, FFloat),
          ((V4 FFloat, V4 FFloat),
           ((V4 FFloat, V4 FFloat),
            ((V4 FFloat, V4 FFloat),
             (V4 FFloat, V4 FFloat)))))))))
  -> V4 FFloat
sampleAndLightWithSimple diffSampler normSampler matSampler shadowSampler
  ((normal, uv, matIdF, worldPos),
   ((dirLightDir, dirLightColorInt),
    ((shadowPos0, shadowPos1),
     ((shadowCol0, shadowCol1),
      ((ambient, numShadowF, shadowFar, shadowBias),
       ((simplePos0, simplePos1),
        ((simplePos2, simplePos3),
         ((simpleCol0, simpleCol1),
          (simpleCol2, simpleCol3))))))))) =
  let -- Determine face direction from normal
      faceIdx = normalToFaceIndex normal

      -- Look up texture layer
      matUV = V2 ((faceIdx + 0.5) / 6.0) ((matIdF + 0.5) / 256.0)
      texLayer = sample2D matSampler SampleAuto Nothing Nothing matUV

      -- Sample textures
      V2 u v = uv
      uvLayer = V3 u v texLayer
      diffuseSrgb = sample2DArray diffSampler SampleAuto Nothing uvLayer
      normalSample = sample2DArray normSampler SampleAuto Nothing uvLayer

      -- Convert to linear
      V4 dr dg db da = diffuseSrgb
      diffuseLinear = V3 (srgbToLinear dr) (srgbToLinear dg) (srgbToLinear db)

      -- Normal mapping
      (tangent, bitangent, _) = computeTBN normal
      shadingNormal = applyNormalMap normal tangent bitangent normalSample

      -- Directional light
      V4 dirR dirG dirB dirIntensity = dirLightColorInt
      dirLightDirNorm = signorm dirLightDir
      V3 nx ny nz = shadingNormal
      dirNdotL = maxB 0 (nx * (dirLightDirNorm^._x) + ny * (dirLightDirNorm^._y) + nz * (dirLightDirNorm^._z))
      dirContrib = V3 (dirNdotL * dirIntensity * dirR)
                      (dirNdotL * dirIntensity * dirG)
                      (dirNdotL * dirIntensity * dirB)

      -- Shadow light contributions
      shadowLightPos0 = shadowPos0 ^. _xyz
      shadowLightPos1 = shadowPos1 ^. _xyz
      shadowPower0 = shadowPos0 ^. _w
      shadowPower1 = shadowPos1 ^. _w

      shadow0 = samplePointShadowArray shadowSampler 0 worldPos shadowLightPos0 shadowFar shadowBias
      shadow1 = samplePointShadowArray shadowSampler 1 worldPos shadowLightPos1 shadowFar shadowBias
      sf0 = ifThenElse' (numShadowF >* 0.5) shadow0 1.0
      sf1 = ifThenElse' (numShadowF >* 1.5) shadow1 1.0

      shadowContrib0 = computePointLightContrib shadingNormal worldPos shadowLightPos0 (shadowCol0 ^. _xyz) shadowPower0 sf0
      shadowContrib1 = computePointLightContrib shadingNormal worldPos shadowLightPos1 (shadowCol1 ^. _xyz) shadowPower1 sf1

      -- Simple light contributions (no shadows, always factor = 1.0)
      -- numSimpleLights is in simpleCol0.w
      numSimpleF = simpleCol0 ^. _w

      simpleContrib0 = ifThenElse' (numSimpleF >* 0.5)
        (computePointLightContribNoShadow shadingNormal worldPos (simplePos0 ^. _xyz) (simpleCol0 ^. _xyz) (simplePos0 ^. _w))
        (V3 0 0 0)
      simpleContrib1 = ifThenElse' (numSimpleF >* 1.5)
        (computePointLightContribNoShadow shadingNormal worldPos (simplePos1 ^. _xyz) (simpleCol1 ^. _xyz) (simplePos1 ^. _w))
        (V3 0 0 0)
      simpleContrib2 = ifThenElse' (numSimpleF >* 2.5)
        (computePointLightContribNoShadow shadingNormal worldPos (simplePos2 ^. _xyz) (simpleCol2 ^. _xyz) (simplePos2 ^. _w))
        (V3 0 0 0)
      simpleContrib3 = ifThenElse' (numSimpleF >* 3.5)
        (computePointLightContribNoShadow shadingNormal worldPos (simplePos3 ^. _xyz) (simpleCol3 ^. _xyz) (simplePos3 ^. _w))
        (V3 0 0 0)

      -- Combine all contributions
      totalPtContrib = shadowContrib0 + shadowContrib1
                     + simpleContrib0 + simpleContrib1 + simpleContrib2 + simpleContrib3

      -- Final lighting
      V3 linR linG linB = diffuseLinear
      V3 ptR ptG ptB = totalPtContrib
      V3 dR dG dB = dirContrib
      litR = linR * (ambient + dR + ptR)
      litG = linG * (ambient + dG + ptG)
      litB = linB * (ambient + dB + ptB)

      -- Convert to sRGB
      outR = linearToSrgb litR
      outG = linearToSrgb litG
      outB = linearToSrgb litB

  in V4 outR outG outB da

-- | Compute point light contribution without shadow (always lit)
computePointLightContribNoShadow
  :: V3 FFloat -> V3 FFloat -> V3 FFloat -> V3 FFloat -> FFloat -> V3 FFloat
computePointLightContribNoShadow normal worldPos lightPos lightColor power =
  computePointLightContrib normal worldPos lightPos lightColor power 1.0

-- | Compute a single point light's contribution
computePointLightContrib
  :: V3 FFloat -> V3 FFloat -> V3 FFloat -> V3 FFloat -> FFloat -> FFloat -> V3 FFloat
computePointLightContrib normal worldPos lightPos lightColor power shadowFactor =
  let toLight = lightPos - worldPos
      dist = norm toLight
      lightDir = signorm toLight
      V3 nx ny nz = normal
      ndotL = maxB 0 (nx * (lightDir^._x) + ny * (lightDir^._y) + nz * (lightDir^._z))
      attenuation = 1.0 / (1.0 + power * dist * dist)
      V3 r g b = lightColor
      contrib = shadowFactor * ndotL * attenuation
  in V3 (contrib * r) (contrib * g) (contrib * b)

-- | Create multi-light shadow context
newPhongAtlasMultiShadowContext
  :: Window os RGBAFloat Depth
  -> TextureAtlas os
  -> MaterialTable
  -> Camera Float
  -> ShadowConfig
  -> SpiderCtx os (PhongAtlasMultiShadowContext os)
newPhongAtlasMultiShadowContext win atlas materials camera shadowCfg = do
  matBuffer <- newBuffer 1

  -- Create material lookup texture
  let (texW, texH, pixels) = materialsToPixels materials
      actualH = max 256 texH
  matLookupTex <- newTexture2D R32F (V2 texW actualH) 1
  writeTexture2D matLookupTex 0 0 (V2 texW texH) pixels

  -- Get or create normal texture
  normalTex <- case atlasNormal atlas of
    Just tex -> pure tex
    Nothing -> do
      let flatNormal :: V4 Float
          flatNormal = V4 0.5 0.5 1.0 1.0
          V2 w h = atlasSize atlas
          numLayers = atlasLayers atlas
      tex <- newTexture2DArray RGBA8 (V3 w h numLayers) 1
      let flatPixels = replicate (w * h) flatNormal
      forM_ [0 .. numLayers - 1] $ \layer ->
        writeTexture2DArray tex 0 (V3 0 0 layer) (V3 w h 1) flatPixels
      pure tex

  -- Create multi-light shadow resources
  shadow <- newMultiPointLightShadow shadowCfg

  -- Compile shaders
  shadowShader <- compileMultiShadowShaders shadow
  mainShader <- compileShader $ phongAtlasMultiShadowShader win matBuffer

  pure PhongAtlasMultiShadowContext
    { pmsWindow       = win
    , pmsMatrix       = matBuffer
    , pmsCamera       = camera
    , pmsMainShader   = mainShader
    , pmsShadowShader = shadowShader
    , pmsTexture      = atlas
    , pmsMaterials    = materials
    , pmsMatLookup    = matLookupTex
    , pmsNormalTex    = normalTex
    , pmsShadow       = shadow
    }

-- | Maximum shadow-casting lights for this implementation
maxMultiShadowLights :: Int
maxMultiShadowLights = 2

-- | Render model with multiple shadow-casting point lights (up to 2) and simple lights (up to 4)
renderModelAtlasMultiShadow
  :: PhongAtlasMultiShadowContext os
  -> SceneModelAtlas os
  -> Transform Float
  -> MultiLighting
  -> SpiderCtx os ()
renderModelAtlasMultiShadow PhongAtlasMultiShadowContext{..} model transform lighting = do
  let shadowLights = take maxMultiShadowLights (multiLightingShadowLights lighting)
      simpleLights = take maxSimpleLights (multiLightingSimpleLights lighting)
      lightPositions = map pointLightPosition shadowLights

  -- Step 1: Render shadow maps for active shadow lights (up to 2)
  renderMultiShadowCubes pmsShadow pmsShadowShader lightPositions transform model

  -- Step 2: Render main scene
  size@(V2 w h) <- getFrameBufferSize pmsWindow
  let newCam = pmsCamera { cameraAspect = fromIntegral w / fromIntegral h }
      modelMat = transformMatrix transform
      viewProjMat = cameraProjMat newCam !*! cameraViewMat newCam !*! modelMat
      normMat = fromQuaternion (transformRotation transform)

      -- Pack directional light
      dirLightPacked = case multiLightingDirectional lighting of
        Just dl -> let V3 dx dy dz = signorm (dirLightDirection dl)
                   in V4 dx dy dz (dirLightIntensity dl)
        Nothing -> V4 0 0 1 0

      -- Pack light helper functions
      packLight pl = let V3 px py pz = pointLightPosition pl
                     in V4 px py pz (pointLightPower pl)
      packColor pl = let V3 r g b = pointLightColor pl
                     in V4 r g b 0
      defaultLight = V4 0 0 0 0

      -- Pack shadow lights (pos.xyz, power)
      shadowPos0 = case shadowLights of
        (l:_) -> packLight l
        []    -> defaultLight
      shadowPos1 = case drop 1 shadowLights of
        (l:_) -> packLight l
        []    -> defaultLight

      -- Pack shadow light colors (color.rgb, unused)
      shadowCol0 = case shadowLights of
        (l:_) -> packColor l
        []    -> defaultLight
      shadowCol1 = case drop 1 shadowLights of
        (l:_) -> packColor l
        []    -> defaultLight

      -- Pack simple light positions (pos.xyz, power)
      simplePos0 = case simpleLights of
        (l:_) -> packLight l
        []    -> defaultLight
      simplePos1 = case drop 1 simpleLights of
        (l:_) -> packLight l
        []    -> defaultLight
      simplePos2 = case drop 2 simpleLights of
        (l:_) -> packLight l
        []    -> defaultLight
      simplePos3 = case drop 3 simpleLights of
        (l:_) -> packLight l
        []    -> defaultLight

      -- Pack simple light colors (color.rgb, numSimpleLights in first w)
      numSimpleLights = fromIntegral (length simpleLights) :: Float
      simpleCol0 = case simpleLights of
        (l:_) -> let V3 r g b = pointLightColor l in V4 r g b numSimpleLights
        []    -> V4 0 0 0 numSimpleLights  -- Still need numSimpleLights even if no lights
      simpleCol1 = case drop 1 simpleLights of
        (l:_) -> packColor l
        []    -> defaultLight
      simpleCol2 = case drop 2 simpleLights of
        (l:_) -> packColor l
        []    -> defaultLight
      simpleCol3 = case drop 3 simpleLights of
        (l:_) -> packColor l
        []    -> defaultLight

      ambient = multiLightingAmbient lighting
      numShadowLights = fromIntegral (length shadowLights) :: Float
      cfg = multiShadowConfig pmsShadow
      params = V4 ambient numShadowLights (shadowFar cfg) (shadowBias cfg)

      shadowSamplerFilter = SamplerFilter Nearest Nearest Nearest Nothing
      shadowEdgeMode = (V2 ClampToEdge ClampToEdge, 1.0)

  writeBuffer pmsMatrix 0
    [(viewProjMat, modelMat, normMat, dirLightPacked, shadowPos0, shadowPos1,
      (shadowCol0, shadowCol1, params,
       (simplePos0, simplePos1, simplePos2, simplePos3,
        (simpleCol0, simpleCol1, simpleCol2, simpleCol3))))]

  render $ do
    clearWindowColor pmsWindow 0
    clearWindowDepth pmsWindow 1
    prims <- model

    pmsMainShader $ PhongAtlasEnvMultiShadow
      { multiShadowPrimitives = prims
      , multiShadowRasterOptions = (FrontAndBack, ViewPort 0 size, DepthRange 0 1)
      , multiShadowDiffuseAtlas = ( atlasDiffuse pmsTexture
                                  , SamplerFilter Nearest Nearest Nearest Nothing
                                  , (V2 Repeat Repeat, V4 0 0 0 0)
                                  )
      , multiShadowNormalAtlas = ( pmsNormalTex
                                 , SamplerFilter Nearest Nearest Nearest Nothing
                                 , (V2 Repeat Repeat, V4 0.5 0.5 1.0 1.0)
                                 )
      , multiShadowMaterialLookup = ( pmsMatLookup
                                    , SamplerFilter Nearest Nearest Nearest Nothing
                                    , (V2 ClampToEdge ClampToEdge, 0)
                                    )
      , multiShadowDepthArray = ( multiShadowArray pmsShadow
                           , shadowSamplerFilter
                           , shadowEdgeMode
                           )
      }
  swapWindowBuffers pmsWindow
