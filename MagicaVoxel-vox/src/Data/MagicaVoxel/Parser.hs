module Data.MagicaVoxel.Parser(
    parseVox
  ) where

import Control.Applicative
import Data.Attoparsec.ByteString
import Data.Bits
import Data.ByteString (ByteString)
import Data.Functor
import Data.MagicaVoxel.Types
import Data.Word

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

parseVox :: ByteString -> Either String VoxFile
parseVox = parseOnly (voxParser <* endOfInput)

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
parsePalette = undefined

parseMaterial :: Parser VoxMaterial
parseMaterial = undefined

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
