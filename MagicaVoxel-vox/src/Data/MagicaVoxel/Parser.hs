module Data.MagicaVoxel.Parser(
    parseFile
  , parseVox
  ) where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Data.Array.ST (newArray, readArray, MArray, STUArray)
import Data.Array.Unsafe (castSTUArray)
import Data.Attoparsec.Binary
import Data.Attoparsec.ByteString as A
import Data.Bits
import Data.ByteString (ByteString)
import Data.Functor
import Data.MagicaVoxel.Types
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Word
import GHC.ST (runST, ST)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

parseFile :: MonadIO m => FilePath -> m (Either String VoxFile)
parseFile = liftIO . fmap parseVox . BS.readFile

parseVox :: ByteString -> Either String VoxFile
parseVox = parseOnly (voxParser <* endOfInput)

voxParser :: Parser VoxFile
voxParser = do
  void $ string "VOX" *> anyWord8
  voxFileVersion <- anyWord32
  chunk_ "MAIN"
  nm <- packChunk
  voxFileModels <- V.replicateM (fromIntegral nm) parseModel
  let voxFilePallete = Nothing 
  let voxFileMaterials = V.empty
  execStateT go VoxFile{..}
  where 
    go = do 
      isEnd <- lift atEnd
      if isEnd then pure ()
      else do 
        name <- lift $ A.take 4
        chunkLen <- lift anyWord32 
        children <- lift anyWord32 
        case name of 
          "nTRN" -> lift . void $ A.take (fromIntegral chunkLen) -- TODO: Transform chunk
          "nGRP" -> lift . void $ do -- TODO: Group chunk
            _ <- A.take (fromIntegral chunkLen) 
            replicateM (fromIntegral children) anyWord32
          "nSHP" -> lift . void $ do -- TODO: Shape Node Chunk
            A.take (fromIntegral chunkLen)
          "LAYR" -> lift . void $ do -- TODO: Layer Chunk
            A.take (fromIntegral chunkLen)
          "RGBA" -> do 
            palette <- lift parsePalette
            modify' $ \s -> s { voxFilePallete = Just palette }
          "MATL" -> lift . void $ do -- TODO: New material chunk
            A.take (fromIntegral chunkLen)
          "rOBJ" -> lift . void $ do -- TODO: Render Objects Chunk
            A.take (fromIntegral chunkLen)
          "SIZE" -> lift . void $ do -- TODO: extra SIZE chunk
            A.take (fromIntegral chunkLen)
          "XYZI" -> lift . void $ do -- TODO: extra XYZI chunk 
            A.take (fromIntegral chunkLen)
          "IMAP" -> lift . void $ do -- TODO: Index MAP Chunk
            A.take (fromIntegral chunkLen)
          _ -> fail $ "Unknown chunk: " ++ BSC.unpack name 
        go 

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
parsePalette = VU.replicateM 256 (toRgba <$> anyWord32le)

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

-- -- | Big endian 4 bytes word parser
anyWord32 :: Parser Word32
anyWord32 = anyWord32le

-- | Parse big endian 4 bytes float
anyFloat :: Parser Float
anyFloat = fmap wordToFloat anyWord32

wordToFloat :: Word32 -> Float
wordToFloat x = runST (cast x)

{-# INLINE cast #-}
cast :: (MArray (STUArray s) a (ST s),
         MArray (STUArray s) b (ST s)) => a -> ST s b
cast x = newArray (0 :: Int, 0) x >>= castSTUArray >>= flip readArray 0
