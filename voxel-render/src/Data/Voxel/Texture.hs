module Data.Voxel.Texture(
    loadTexture2D
  ) where 

import Codec.Picture (readImage, convertRGBA8, imageWidth, imageHeight, imageData)
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Voxel.App 
import Data.Word 
import Graphics.GPipe 

import qualified Data.Vector.Storable as V 

-- | Load texture from file, using JuicyPixels with dynamic conversion 
-- between formats.
loadTexture2D :: FilePath -> SpiderCtx os (Either String (Texture2D os (Format RGBAFloat)))
loadTexture2D name = runExceptT $ do 
  imgDyn <- ExceptT $ liftIO $ readImage name
  lift $ do 
    let img = convertRGBA8 imgDyn 
    let width = imageWidth img
    let height = imageHeight img
    let size = V2 width height
    let pixels = packColors $ imageData img
    tex <- newTexture2D RGBA8 size 1 
    writeTexture2D tex 0 0 size pixels
    pure tex

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
