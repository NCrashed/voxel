module Data.Voxel.Mesh.Mutable.Internal where

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
  => Int -- ^ Amount of vertecies
  -> Int -- ^ Amount of triangles
  -> m (MMesh (PrimState m) a)
new vn tn = MMesh
  <$> MV.new vn
  <*> MV.new vn
  <*> MV.new vn
  <*> MV.new (tn*3)
  <*> MV.new vn
{-# INLINE new #-}

-- | Write single vertex into mesh
writeVertex :: (PrimMonad m, Storable a)
  => MMesh (PrimState m) a
  -> Int -- ^ Index
  -> V3 Float -- ^ Position
  -> V3 Float -- ^ Normal
  -> V2 Float -- ^ Uv
  -> a -- ^ Data
  -> m ()
writeVertex MMesh{..} i p n u a = do
  MV.write mmeshVertices i p
  MV.write mmeshNormals i n
  MV.write mmeshUvs i u
  MV.write mmeshData i a

-- | Write triangle to the
writeIndex :: (PrimMonad m, Storable a)
  => MMesh (PrimState m) a
  -> Int -- ^ Index
  -> V3 Word32
  -> m ()
writeIndex MMesh{..} i (V3 a b c) = do
  MV.write mmeshIndecies (i*3) a
  MV.write mmeshIndecies (i*3+1) b
  MV.write mmeshIndecies (i*3+2) c
