module Data.Voxel.Mesh.Internal where

import Control.Monad.ST
import Data.Vector.Storable (Vector, Storable)
import Data.Voxel.Mesh.Mutable.Internal (MMesh)
import Data.Word
import Linear

import qualified Data.Vector.Storable as V
import qualified Data.Voxel.Mesh.Mutable.Internal as MM

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

-- | Create mesh from operations on mutable mesh, freezes results of the monadic
-- action.
create :: Storable a => (forall s . ST s (MMesh s a)) -> Mesh a
{-# INLINE create #-}
create action = runST $ do
  MM.MMesh{..} <- action
  Mesh <$> V.unsafeFreeze mmeshVertices
       <*> V.unsafeFreeze mmeshNormals
       <*> V.unsafeFreeze mmeshUvs
       <*> V.unsafeFreeze mmeshIndecies
       <*> V.unsafeFreeze mmeshData
