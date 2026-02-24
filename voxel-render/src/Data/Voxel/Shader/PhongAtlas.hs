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

-- | Uniform buffer for MVP matrix and normal matrix (same as Phong shader)
type PhongAtlasUniform os = Buffer os (Uniform (V4 (B4 Float), V3 (B3 Float)))

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
  (modelViewProj, normMat) <- getUniform (const (uniform, 0))
  let projectedSides = proj modelViewProj normMat <$> sides

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

-- | Project vertex to clip space, outputting data for fragment shader
proj :: V4 (V4 VFloat)
     -> V3 (V3 VFloat)
     -> MeshVertex VInt
     -> (V4 VFloat, (V3 FlatVFloat, V2 VFloat, VFloat))
proj modelViewProj normMat MeshVertex{..} =
  let V3 px py pz = meshPrimPosition
      clipPos = modelViewProj !* V4 px py pz 1
      worldNormal = normMat !* meshPrimNormal
      -- Convert material ID to float for passing to fragment shader
      matIdFloat = toFloat meshPrimData
  in (clipPos, (fmap Flat worldNormal, meshPrimUv, matIdFloat))

-- | Sample texture and apply Phong lighting with normal mapping
sampleAndLight
  :: Sampler2DArray (Format RGBAFloat)
  -> Sampler2DArray (Format RGBAFloat)
  -> Sampler2D (Format RFloat)
  -> (V3 FFloat, V2 FFloat, FFloat)  -- ^ (normal, uv, materialId as float)
  -> V4 FFloat
sampleAndLight diffSampler normSampler matSampler (normal, uv, matIdF) =
  let -- Determine face direction from normal (0-5)
      faceIdx = normalToFaceIndex normal

      -- Look up texture layer from material lookup texture
      matUV = V2 ((faceIdx + 0.5) / 6.0) ((matIdF + 0.5) / 256.0)
      texLayer = sample2D matSampler SampleAuto Nothing Nothing matUV

      -- Sample from texture array using UV and layer
      V2 u v = uv
      uvLayer = V3 u v texLayer
      diffuse = sample2DArray diffSampler SampleAuto Nothing uvLayer
      normalSample = sample2DArray normSampler SampleAuto Nothing uvLayer

      -- Compute TBN matrix for this face
      (tangent, bitangent, _) = computeTBN normal

      -- Apply normal map to get perturbed normal
      shadingNormal = applyNormalMap normal tangent bitangent normalSample
      V3 nx ny nz = shadingNormal

      -- Apply Phong lighting with hardcoded directional light
      lightDir = signorm $ V3 (-1) (-1) 1
      nDotL = maxB 0 (nx * (lightDir^._x) + ny * (lightDir^._y) + nz * (lightDir^._z))
      ambient = 0.3
      lighting = ambient + nDotL * 0.7

      -- Apply lighting to texture
      V4 dr dg db da = diffuse
      lit = V4 (dr * lighting) (dg * lighting) (db * lighting) da

  in lit

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
  -> SpiderCtx os ()
renderModelAtlas PhongAtlasContext{..} model transform = do
  -- Write this frame's uniform value
  size@(V2 w h) <- getFrameBufferSize phongAtlasWindow
  let newCam = phongAtlasCamera { cameraAspect = fromIntegral w / fromIntegral h }
      modelMat = transformMatrix transform
      viewProjMat = cameraProjMat newCam !*! cameraViewMat newCam !*! modelMat
      normMat = fromQuaternion (transformRotation transform)

  writeBuffer phongAtlasMatrix 0 [(viewProjMat, normMat)]

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
