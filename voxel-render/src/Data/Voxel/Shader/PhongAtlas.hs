{-# LANGUAGE NoImplicitPrelude #-}
-- | Phong shader with texture atlas support using 2D texture arrays
module Data.Voxel.Shader.PhongAtlas(
    PhongAtlasEnv(..)
  , PhongAtlasUniform
  , phongAtlasShader
  , PhongAtlasContext(..)
  , newPhongAtlasContext
  , renderModelAtlas
  , SceneModelAtlas
  , DirectionalLight(..)
  , PointLight(..)
  , Lighting(..)
  , noLight
  , ambientOnly
  , directionalOnly
  , pointOnly
  ) where

import Control.Lens ((^.))
import Control.Monad (forM_)
import Data.Voxel.App
import Data.Voxel.Camera
import Data.Voxel.GPipe.Mesh
import Data.Voxel.Material
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

-- | Uniform buffer for MVP matrix, normal matrix, and lighting
-- Contains: (MVP matrix, Normal matrix, (DirLight direction, DirLight color+intensity),
--            (PointLight position, PointLight color+power), ambient)
type PhongAtlasUniform os = Buffer os (Uniform
  ( V4 (B4 Float)  -- MVP matrix
  , V3 (B3 Float)  -- Normal matrix
  , B3 Float       -- Directional light direction
  , B4 Float       -- Directional light color (RGB) + intensity (A)
  , B3 Float       -- Point light position
  , B4 Float       -- Point light color (RGB) + power/attenuation (A)
  , B Float        -- Ambient intensity
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
  (modelViewProj, normMat, dirLightDir, dirLightColorInt, pointLightPos, pointLightColorPow, ambient)
    <- getUniform (const (uniform, 0))
  -- Pass light params through vertex shader as Flat values
  let projectedSides = proj modelViewProj normMat
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
     -> V3 (V3 VFloat)      -- ^ Normal matrix
     -> V3 VFloat           -- ^ Directional light direction
     -> V4 VFloat           -- ^ Directional light color + intensity
     -> V3 VFloat           -- ^ Point light position
     -> V4 VFloat           -- ^ Point light color + power
     -> VFloat              -- ^ Ambient
     -> MeshVertex VInt
     -> (V4 VFloat, ((V3 FlatVFloat, V2 VFloat, VFloat, V3 VFloat),
                     (V3 FlatVFloat, V4 FlatVFloat, V3 FlatVFloat, V4 FlatVFloat, FlatVFloat)))
proj modelViewProj normMat dirLightDir dirLightColorInt pointLightPos pointLightColorPow ambient MeshVertex{..} =
  let V3 px py pz = meshPrimPosition
      worldPos = V3 px py pz  -- World position for point light calc
      clipPos = modelViewProj !* V4 px py pz 1
      worldNormal = normMat !* meshPrimNormal
      matIdFloat = toFloat meshPrimData
      -- Geometry data
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

      -- Pack directional light parameters (zero if disabled)
      (dirLightDir, dirLightColorInt) = case lightingDirectional lighting of
        Just dl -> ( signorm (dirLightDirection dl)
                   , let V3 r g b = dirLightColor dl
                     in V4 r g b (dirLightIntensity dl)
                   )
        Nothing -> (V3 0 0 1, V4 0 0 0 0)  -- Disabled

      -- Pack point light parameters (zero power if disabled)
      (pointLightPos, pointLightColorPow) = case lightingPoint lighting of
        Just pl -> ( pointLightPosition pl
                   , let V3 r g b = pointLightColor pl
                     in V4 r g b (pointLightPower pl)
                   )
        Nothing -> (V3 0 0 0, V4 0 0 0 0)  -- Disabled

      ambient = lightingAmbient lighting

  writeBuffer phongAtlasMatrix 0
    [(viewProjMat, normMat, dirLightDir, dirLightColorInt, pointLightPos, pointLightColorPow, ambient)]

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
