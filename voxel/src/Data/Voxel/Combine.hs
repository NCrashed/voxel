module Data.Voxel.Combine(
    CombineVoxel(..)
  ) where

import Linear
import Data.Voxel.Empty
import Data.List (foldl1')

-- | Indicates that 4 voxel can be combined into one single voxel
class CombineVoxel a where
  -- | Combine 8 voxels into single one. 
  -- 
  -- Here the indexing map of voxels to arguments:
  -- ```
  -- For plane Z=0
  -- Y\X | 0  | 1
  -- --------------
  -- 0   | v0 | v1
  -- 1   | v2 | v3
  --
  -- For plane Z=1
  -- Y\X | 0  | 1
  -- --------------
  -- 0   | v4 | v5
  -- 1   | v6 | v7
  -- ``` 
  combineCube :: a -> a -> a -> a -> a -> a -> a -> a -> a

  -- | Way to mix two voxel and get a average of them.
  combineVoxel :: a -> a -> a 

instance {-# OVERLAPPABLE #-} (CombineVoxel a, Num a, Eq a) => CombineVoxel (V3 a) where 
  combineCube v0 v1 v2 v3 v4 v5 v6 v7 
    | empties > (4::Int) = emptyVoxel
    | otherwise = foldl1' combineVoxel notEmpties
    where 
      vs = [v0, v1, v2, v3, v4, v5, v6, v7]
      notEmpties = filter (not . isEmptyVoxel) vs
      empties = sum . map (const 1) . filter id . fmap isEmptyVoxel $ vs
  {-# INLINE combineCube #-}

  combineVoxel (V3 x1 y1 z1) (V3 x2 y2 z2) = V3 (combineVoxel x1 x2) (combineVoxel y1 y2) (combineVoxel z1 z2) 
  {-# INLINE combineVoxel #-}

instance CombineVoxel Float where 
  combineCube v0 v1 v2 v3 v4 v5 v6 v7 
    = (v0 + v1 + v2 + v3 + v4 + v5 + v6 + v7) / 8.0
  {-# INLINE combineCube #-}

  combineVoxel a b = (a + b) / 2.0
  {-# INLINE combineVoxel #-}

instance CombineVoxel Double where 
  combineCube v0 v1 v2 v3 v4 v5 v6 v7 
    = (v0 + v1 + v2 + v3 + v4 + v5 + v6 + v7) / 8.0
  {-# INLINE combineCube #-}

  combineVoxel a b = (a + b) / 2.0
  {-# INLINE combineVoxel #-}
