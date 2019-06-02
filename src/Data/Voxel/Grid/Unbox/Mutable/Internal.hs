module Data.Voxel.Grid.Unbox.Mutable.Internal where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST (RealWorld)
import Data.Vector.Unboxed.Mutable (Unbox, MVector)
import Linear

import qualified Data.Vector.Unboxed.Mutable as VM

-- | Mutable square 3D grid of unboxed data. The 's' type variable is phantom
-- type that catches world state like in `ST` monad, so you can use the storage
-- in 'IO' and 'ST' contexts.
--
-- Effectively it is fixed size wrapper around `MVector`.
data MVoxelGrid s a = MVoxelGrid {
  -- | Size of one dimension
    voxelGridSize :: !Int
  -- | Vector storage of grid
  , voxelGridData :: !(MVector s a)
}

-- | 'MVoxelGrid' for 'IO' monad
type IOVoxelGrid = MVoxelGrid RealWorld
-- | 'MVoxelGrid' for 'ST' monad. The type synonym exists only for uniform API looking
-- with 'IOVoxelGrid'
type STVoxelGrid s = MVoxelGrid s

-- | Allocate new grid with given size of each of 3 dimensions.
new :: (PrimMonad m, Unbox a) => Int -> m (MVoxelGrid (PrimState m) a)
new n = do
  s <- VM.new (n * n * n)
  pure $! MVoxelGrid n s
{-# INLINE new #-}

-- | Allocate new grid with given size of each of 3 dimension with given value.
replicate :: (PrimMonad m, Unbox a) => Int -> a -> m (MVoxelGrid (PrimState m) a)
replicate n a0 = do
  s <- VM.replicate (n * n * n) a0
  pure $! MVoxelGrid n s
{-# INLINE replicate #-}

-- | Allocate new grid with given size of each of 3 dimension with given value.
replicateM :: (PrimMonad m, Unbox a) => Int -> m a -> m (MVoxelGrid (PrimState m) a)
replicateM n ma = do
  s <- VM.replicateM (n * n * n) ma
  pure $! MVoxelGrid n s
{-# INLINE replicateM #-}

-- | Copy memory from given grid to new grid.
clone :: (PrimMonad m, Unbox a) => MVoxelGrid (PrimState m) a -> m (MVoxelGrid (PrimState m) a)
clone (MVoxelGrid n a) = do
  b <- VM.clone a
  pure $! MVoxelGrid n b
{-# INLINE clone #-}

-- | Get size of single dimension of the grid
size :: MVoxelGrid s a -> Int
size = voxelGridSize
{-# INLINE size #-}

-- | Get amount of voxels inside the grid.
length :: MVoxelGrid s a -> Int
length g = s * s * s
  where
    s = size g
{-# INLINE length #-}

-- | Convert 3d position in grid to index in flat array
posToIndex ::
     Int -- ^ Dimension size
  -> V3 Int -- ^ Position
  -> Int -- ^ Index in array
posToIndex n (V3 x y z) = x + y * n + z * n * n

-- | Convert index in flat array to grid 3d position
indexToPos ::
     Int -- ^ Dimension size
  -> Int -- ^ Index in array
  -> V3 Int -- ^ Position
indexToPos n i = V3 x y z
  where
    x = i' `mod` n
    y = i' `div` n
    z = i `div` (n*n)
    i' = i - z * n * n

-- | Read single element in grid with bounding checks.
read :: (PrimMonad m, Unbox a) => MVoxelGrid (PrimState m) a -> V3 Int -> m a
read (MVoxelGrid n g) p = do
  let i = posToIndex n p
  VM.read g i
