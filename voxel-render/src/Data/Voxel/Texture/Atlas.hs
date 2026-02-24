-- | Texture atlas support using 2D texture arrays
module Data.Voxel.Texture.Atlas(
    TextureAtlas(..)
  , loadTextureAtlas
  , loadTextureAtlasWithNormals
  ) where

import Codec.Picture (readImage, convertRGBA8, imageWidth, imageHeight, imageData)
import Control.Monad (forM, forM_, when)
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Voxel.App
import Data.Word
import Graphics.GPipe
import Linear

import qualified Data.Vector.Storable as V

-- | Texture atlas containing 2D array textures for diffuse and optional normal maps
data TextureAtlas os = TextureAtlas
  { atlasDiffuse :: !(Texture2DArray os (Format RGBAFloat))
    -- ^ Diffuse color texture array
  , atlasNormal  :: !(Maybe (Texture2DArray os (Format RGBAFloat)))
    -- ^ Optional normal map texture array
  , atlasLayers  :: !Int
    -- ^ Number of texture layers in the atlas
  , atlasSize    :: !(V2 Int)
    -- ^ Size of each texture layer (width x height)
  }

-- | Load texture layers from list of file paths.
-- All images must be the same size.
loadTextureAtlas
  :: [FilePath]           -- ^ Diffuse texture paths (one per layer)
  -> SpiderCtx os (Either String (TextureAtlas os))
loadTextureAtlas diffusePaths = loadTextureAtlasWithNormals diffusePaths Nothing

-- | Load texture layers with optional normal maps.
-- All images must be the same size within their respective sets.
loadTextureAtlasWithNormals
  :: [FilePath]           -- ^ Diffuse texture paths (one per layer)
  -> Maybe [FilePath]     -- ^ Optional normal map paths (must match diffuse count)
  -> SpiderCtx os (Either String (TextureAtlas os))
loadTextureAtlasWithNormals diffusePaths mNormalPaths = runExceptT $ do
  when (null diffusePaths) $
    throwError "No diffuse textures provided"

  -- Load diffuse textures
  diffuseImages <- forM diffusePaths $ \path -> do
    imgDyn <- ExceptT $ liftIO $ readImage path
    pure $ convertRGBA8 imgDyn

  -- Verify all diffuse images are same size
  let firstImg = head diffuseImages
      width = imageWidth firstImg
      height = imageHeight firstImg
      size = V2 width height
      numLayers = length diffuseImages

  forM_ (zip [1..] diffuseImages) $ \(i :: Int, img) ->
    when (imageWidth img /= width || imageHeight img /= height) $
      throwError $ "Diffuse texture " ++ show i ++ " has different size than first texture"

  -- Create diffuse texture array
  diffuseTex <- lift $ do
    tex <- newTexture2DArray RGBA8 (V3 width height numLayers) 1
    forM_ (zip [0..] diffuseImages) $ \(layer, img) -> do
      let pixels = packColors $ imageData img
      writeTexture2DArray tex 0 (V3 0 0 layer) (V3 width height 1) pixels
    pure tex

  -- Load optional normal maps
  normalTex <- case mNormalPaths of
    Nothing -> pure Nothing
    Just normalPaths -> do
      when (length normalPaths /= numLayers) $
        throwError $ "Normal map count (" ++ show (length normalPaths)
                  ++ ") doesn't match diffuse count (" ++ show numLayers ++ ")"

      normalImages <- forM normalPaths $ \path -> do
        imgDyn <- ExceptT $ liftIO $ readImage path
        pure $ convertRGBA8 imgDyn

      -- Verify normal maps are same size as diffuse
      forM_ (zip [1..] normalImages) $ \(i :: Int, img) ->
        when (imageWidth img /= width || imageHeight img /= height) $
          throwError $ "Normal map " ++ show i ++ " has different size than diffuse textures"

      tex <- lift $ do
        t <- newTexture2DArray RGBA8 (V3 width height numLayers) 1
        forM_ (zip [0..] normalImages) $ \(layer, img) -> do
          let pixels = packColors $ imageData img
          writeTexture2DArray t 0 (V3 0 0 layer) (V3 width height 1) pixels
        pure t
      pure $ Just tex

  pure TextureAtlas
    { atlasDiffuse = diffuseTex
    , atlasNormal  = normalTex
    , atlasLayers  = numLayers
    , atlasSize    = size
    }

-- | Pack raw RGBA8 pixel data into GPU-uploadable format
packColors :: V.Vector Word8 -> [V4 Float]
packColors v
  | V.length v < 4 = []
  | otherwise = word32Color (V4 r g b a) : packColors end
  where
    (start, end) = V.splitAt 4 v
    [r, g, b, a] = V.toList start
{-# INLINABLE packColors #-}

word32Color :: V4 Word8 -> V4 Float
word32Color (V4 r g b a) = V4 r' g' b' a'
  where
    r' = fromIntegral r / 255.0
    g' = fromIntegral g / 255.0
    b' = fromIntegral b / 255.0
    a' = fromIntegral a / 255.0
{-# INLINE word32Color #-}
