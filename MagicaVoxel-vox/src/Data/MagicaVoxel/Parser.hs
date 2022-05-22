module Data.MagicaVoxel.Parser(
    parseFile
  , parseVox
  ) where

import Control.Applicative
import Control.Monad.IO.Class
import Data.Array.ST (newArray, readArray, MArray, STUArray)
import Data.Array.Unsafe (castSTUArray)
import Data.Attoparsec.ByteString
import Data.Bits
import Data.ByteString (ByteString)
import Data.Functor
import Data.MagicaVoxel.Types
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Word
import GHC.ST (runST, ST)

import qualified Data.ByteString as BS
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

parseFile :: MonadIO m => FilePath -> m (Either String VoxFile)
parseFile = liftIO . fmap parseVox . BS.readFile

parseVox :: ByteString -> Either String VoxFile
parseVox = parseOnly (voxParser ) -- <* endOfInput

voxParser :: Parser VoxFile
voxParser = do
  void $ string "VOX" *> anyWord8
  voxFileVersion <- anyWord32
  chunk_ "MAIN"
  nm <- packChunk
  voxFileModels <- V.replicateM (fromIntegral nm) parseModel
  voxFilePallete <- optional parsePalette
  voxFileMaterials <- V.fromList <$> many parseMaterial
  pure VoxFile{..}

-- | Parse chunk with amount of models in file
packChunk :: Parser Word32
packChunk = (chunk_ "PACK" *> anyWord32) <|> pure 1

parseModel :: Parser VoxModel
parseModel = do
  (x, y, z) <- parseSize
  chunk_ "XYZI"
  n <- anyWord32
  vs <- VU.replicateM (fromIntegral n) peekVoxel
  pure $ VoxModel x y z vs

parseSize :: Parser (Word32, Word32, Word32)
parseSize = chunk_ "SIZE" *> ((,,) <$> anyWord32 <*> anyWord32 <*> anyWord32)

peekVoxel :: Parser Voxel
peekVoxel = Voxel <$> anyWord8 <*> anyWord8 <*> anyWord8 <*> anyWord8

parsePalette :: Parser VoxPalette
parsePalette = do
  chunk_ "RGBA"
  VU.replicateM 256 anyWord32

parseMaterial :: Parser VoxMaterial
parseMaterial = do
  chunk_ "MATT"
  voxMaterialId <- anyWord32
  voxMaterialType <- parseMatType
  voxMaterialWeight <- anyFloat
  pbs <- parsePropBits
  propVals <- VU.replicateM 4 anyFloat
  let voxMaterialProps = fillProps pbs propVals
  pure VoxMaterial{..}
  where
    parseMatType :: Parser VoxMaterialType
    parseMatType = do
      i <- anyWord32
      pure $ case i of
        0 -> VMDiffuse
        1 -> VMMetal
        2 -> VMGlass
        3 -> VMEmissive
        _ -> VMDiffuse

    parsePropBits :: Parser (VU.Vector VoxMaterialProperty)
    parsePropBits = do
      i <- anyWord32
      let pbit bi v = if testBit i bi then Just v else Nothing
      pure $ VU.fromList . catMaybes $ [
          pbit 0 $ PlasticProp 0
        , pbit 1 $ RoughnessProp 0
        , pbit 2 $ SpecularProp 0
        , pbit 3 $ IORProp 0
        , pbit 4 $ AttenuationProp 0
        , pbit 5 $ PowerProp 0
        , pbit 6 $ GlowProp 0
        , pbit 7   TotalPowerProp
        ]

    fillProps :: VU.Vector VoxMaterialProperty -> VU.Vector Float -> Set VoxMaterialProperty
    fillProps = undefined

-- | Result elimating `chunk`
chunk_ :: ByteString -> Parser ()
chunk_ = void . chunk

-- | Parse header of chunk and return its length and childs count. Need 4 symbols in name
chunk :: ByteString -> Parser (Word32, Word32)
chunk name = do
  _ <- string name
  n <- anyWord32
  k <- anyWord32
  pure (n, k)

-- | Big endian 4 bytes word parser
anyWord32 :: Parser Word32
anyWord32 = do
  a <- anyWord8
  b <- anyWord8
  c <- anyWord8
  d <- anyWord8
  let sl i = shiftL i . fromIntegral
  pure $ fromIntegral a + sl 8 b + sl 16 c + sl 32 d

-- | Parse big endian 4 bytes float
anyFloat :: Parser Float
anyFloat = fmap wordToFloat anyWord32

wordToFloat :: Word32 -> Float
wordToFloat x = runST (cast x)

{-# INLINE cast #-}
cast :: (MArray (STUArray s) a (ST s),
         MArray (STUArray s) b (ST s)) => a -> ST s b
cast x = newArray (0 :: Int, 0) x >>= castSTUArray >>= flip readArray 0
