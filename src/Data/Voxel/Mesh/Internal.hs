module Data.Voxel.Mesh.Internal(
    Mesh(..)
  ) where

import Data.Vector.Storable (Vector)
import Data.Word
import Linear

-- | Mesh is set of triangles with attached data to each vertex. The mesh is
-- defined with storable vectors to simplify passing data to graphical pipeline.
data Mesh a = Mesh {
  -- | Triangles position. Length must be factor of 3.
  meshVertices :: !(Vector (V3 Float))
  -- | Triangles normals. Length must be factor of 3.
, meshNormals  :: !(Vector (V3 Float))
  -- | Triangles UV coordinates. Lenght must be factor of 3.
, meshUvs      :: !(Vector (V2 Float))
  -- | Indecies of triangles. Length must be factor of 3.
, meshIndecies :: !(Vector Word32)
  -- | Additional data that is attached to each vertex. Length must be factor of 3.
  -- Usually it is color data.
, meshData     :: !(Vector a)
}
