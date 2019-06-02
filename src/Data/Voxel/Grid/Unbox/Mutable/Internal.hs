module Data.Voxel.Grid.Unbox.Mutable.Internal where

import Data.Vector.Unboxed.Mutable (MVector)

newtype MVoxelGrid s a = MVoxelGrid { unMVoxelGrid :: MVector s a }
