module Data.Voxel.Generator(
    generateMap
  ) where 

import Data.Bits 
import Data.Foldable 
import Data.Maybe 
import Data.STRef
import Linear

import qualified Data.Voxel.Grid.Unbox as G 
import qualified Data.Voxel.Grid.Unbox.Mutable as GM 
import qualified Math.Noise as N 

xorshf96Init :: V3 Int 
xorshf96Init = V3 123456789 362436069 521288629

-- | Fast Marsaglia's xorshf generator. Period 2^96-1. 
-- Use z component as resulted random value.
xorshf96 :: V3 Int -> V3 Int
xorshf96 (V3 x0 y0 z0) = (V3 x' y' z')
  where 
    x1 = x0 `xor` (x0 `unsafeShiftL` 16)
    x2 = x1 `xor` (x1 `unsafeShiftR` 5)
    x3 = x2 `xor` (x2 `unsafeShiftL` 1)

    x' = y0
    y' = z0
    z' = x3 `xor` x' `xor` y' 

generateMap :: G.VoxelGrid (V3 Float)
generateMap = G.create $ do 
  let n = 128
  let maxz = 32
  let scale = 0.005
  m <- GM.new (V3 n n maxz)
  r <- newSTRef xorshf96Init
  forM_ [(x, y)| x <- [0 .. n-1], y <- [0 .. n-1]] $ \(x, y) -> do
    let zv = fromMaybe 0 $ N.getValue N.perlin (scale * fromIntegral x, scale * fromIntegral y, 1.0) 
    let mz = fromIntegral maxz
    let z = round $ max 0 $ min (mz - 1) $ mz * 0.5 + zv * mz
    forM_ [0 .. z] $ \i -> do  
      V3 rx _ rz <- readSTRef r 
      modifySTRef' r xorshf96 
      let dv = if rx `mod` 2 == 0 then 0   
            else 0.05 * fromIntegral rz / (fromIntegral (maxBound :: Int))
      let col = V3 (1.0 + dv) (158 / 255 + dv) (68 / 255 + dv)
      GM.write m (V3 x y i) col
  pure m