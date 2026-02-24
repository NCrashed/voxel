module Data.Voxel.Empty(
    EmptyVoxel(..)
  , isEmptyVoxel
  ) where

import Data.Int
import Linear

class EmptyVoxel a where
  emptyVoxel :: a

isEmptyVoxel :: (EmptyVoxel a, Eq a) => a -> Bool
isEmptyVoxel = (== emptyVoxel)

instance {-# OVERLAPPABLE #-} Num a => EmptyVoxel (V3 a) where
  emptyVoxel = 0
  {-# INLINE emptyVoxel #-}

instance EmptyVoxel Int32 where
  emptyVoxel = 0
  {-# INLINE emptyVoxel #-}
