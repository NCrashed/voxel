-- | Conversion uboxed voxel grid to polygons (triangles)
module Data.Voxel.Grid.Unbox.Polygon(
    triangulate
  ) where

import Data.Vector.Unboxed (Unbox)
import Data.Voxel.Grid.Unbox.Internal (VoxelGrid)
import Data.Voxel.Mesh (Mesh)

import qualified Data.Voxel.Mesh as M
import qualified Data.Voxel.Mesh.Mutable as MM

-- | Convert each voxel to cubes build from triangles and return them. Usually
-- 'a' parameter used for color passing.
triangulate :: Unbox a => VoxelGrid a -> Mesh a
triangulate = undefined
-- triangulate g = M.create $ do
--   let n = trianglesCount g
--   mesh <- MM.new 3
--
--   pure mesh
