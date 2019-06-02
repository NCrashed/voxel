module Data.Voxel.Grid.Unbox.Mutable.Internal where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST (RealWorld)
import Data.Vector.Unboxed.Mutable (Unbox, MVector)

import qualified Data.Vector.Unboxed.Mutable as VM

-- | Mutable square 3D grid of unboxed data. The 's' type variable is phantom
-- type that catches world state like in `ST` monad, so you can use the storage
-- in 'IO' and 'ST' contexts.
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

-- | Allocate new grid with given size of any of 3 dimensions.
new :: (PrimMonad m, Unbox a) => Int -> m (MVoxelGrid (PrimState m) a)
new n = do
  s <- VM.new n
  pure $! MVoxelGrid n s
{-# INLINE new #-}

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
