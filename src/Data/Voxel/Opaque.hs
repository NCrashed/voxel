module Data.Voxel.Opaque(
    OpaqueVoxel(..)
  ) where

import Linear

class OpaqueVoxel a where
  isFullyOpaque :: a -> Bool

instance OpaqueVoxel (V3 a) where
  isFullyOpaque = const True
  {-# INLINE isFullyOpaque #-}

instance OpaqueVoxel (V4 Float) where
  isFullyOpaque (V4 _ _ _ a) = a == 1.0
  {-# INLINE isFullyOpaque #-}
