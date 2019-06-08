-- | Conversion uboxed voxel grid to polygons (triangles)
module Data.Voxel.Grid.Unbox.Polygon(
    triangulate
  ) where

import Data.Vector.Unboxed (Unbox)
import Data.Voxel.Grid.Unbox.Internal (VoxelGrid)
import Data.Voxel.Mesh (Mesh)

-- | Convert each voxel to cubes build from triangles and return them. Usually
-- 'a' parameter used for color passing.
triangulate :: Unbox a => VoxelGrid a -> Mesh a
triangulate = undefined
