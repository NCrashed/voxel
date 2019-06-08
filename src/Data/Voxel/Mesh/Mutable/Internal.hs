module Data.Voxel.Mesh.Mutable.Internal(
    MMesh(..)
  , new
  ) where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Vector.Storable.Mutable (MVector, Storable)
import Data.Word
import Linear

import qualified Data.Vector.Storable.Mutable as MV

-- | Mesh is set of triangles with attached data to each vertex. The mesh is
-- defined with storable vectors to simplify passing data to graphical pipeline.
data MMesh s a = MMesh {
  -- | Triangles position. Length must be factor of 3.
  mmeshVertices :: !(MVector s (V3 Float))
  -- | Triangles normals. Length must be factor of 3.
, mmeshNormals  :: !(MVector s (V3 Float))
  -- | Triangles UV coordinates. Lenght must be factor of 3.
, mmeshUvs      :: !(MVector s (V2 Float))
  -- | Indecies of triangles. Length must be factor of 3.
, mmeshIndecies :: !(MVector s Word32)
  -- | Additional data that is attached to each vertex. Length must be factor of 3.
  -- Usually it is color data.
, mmeshData     :: !(MVector s a)
}

-- | Allocate new mesh buffers of given size
new :: (PrimMonad m, Storable a)
  => Int -- ^ Amount of triangles
  -> m (MMesh (PrimState m) a)
new n = MMesh
  <$> MV.new k
  <*> MV.new k
  <*> MV.new k
  <*> MV.new k
  <*> MV.new k
  where
    k = n * 3
