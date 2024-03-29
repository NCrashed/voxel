{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Voxel.GPipe.Mesh(
    MeshBuffers(..)
  , MeshArray(..)
  , MeshVertex(..)
  , MeshBufferExt(..)
  , newMeshBuffers
  , writeMeshBuffers
  , meshBuffers
  , meshBufferArray
  ) where

import Control.Arrow
import Control.Monad
import Control.Monad.IO.Class
import Data.Int
import Data.Kind (Type)
import Data.Proxy
import Data.Vector.Storable (Vector)
import Data.Voxel.Mesh (Mesh)
import Data.Word
import GHC.Generics
import Graphics.GPipe

import qualified Data.Vector.Storable as V
import qualified Data.Voxel.Mesh as M

-- | Structure that holds all buffers for `Mesh`
data MeshBuffers os a = MeshBuffers {
-- | Holds triangles vertecies
  meshBuffPositions :: !(Buffer os (B3 Float))
-- | Holds triangles normals
, meshBuffNormals   :: !(Buffer os (B3 Float))
-- | Holds triangles uv coords
, meshBuffUvs       :: !(Buffer os (B2 Float))
-- | Holds triples of indecies, each index corresponds to elements
-- from the previous buffers.
, meshBuffIndecies  :: !(Buffer os (B Word32))
-- | Additional buffers
, meshBuffData      :: !a
} deriving (Eq, Generic)

-- | Structure that is used inside shader primitive array
data MeshArray a = MeshArray {
  meshArrPosition :: !(B3 Float)
, meshArrNormal   :: !(B3 Float)
, meshArrUv       :: !(B2 Float)
, meshArrData     :: !a
}

-- | Structure that is used inside vertex shader
data MeshVertex a = MeshVertex {
  meshPrimPosition :: !(V3 VFloat)
, meshPrimNormal   :: !(V3 VFloat)
, meshPrimUv       :: !(V2 VFloat)
, meshPrimData     :: !a
}

instance VertexInput a => VertexInput (MeshArray a) where
  type VertexFormat (MeshArray a) = MeshVertex (VertexFormat a)
  toVertex = proc ~(MeshArray p n t a) -> do
    p' <- toVertex -< p
    n' <- toVertex -< n
    t' <- toVertex -< t
    a' <- toVertex -< a
    returnA -< MeshVertex p' n' t' a'

-- | Operations for extending buffers of mesh
class MeshBufferExt a where
  -- | Maps from mesh data element to buffer type
  type BufferOf os a :: Type
  -- | Maps from mesh data element to primitive array element
  type ArrayOf a :: Type

  -- | Allocate additional buffers for `MeshBuffers`
  newMeshDataBuffer :: (MonadIO m, ContextHandler ctx)
    => Proxy a
    -> Int -- ^ Count of vertecies
    -> Int -- ^ Count of triangles
    -> ContextT ctx os m (BufferOf os a)

  -- | Write down mesh data to buffer
  writeMeshDataBuffer :: (MonadIO m, ContextHandler ctx)
    => BufferOf os a
    -> BufferStartPos
    -> Vector a
    -> ContextT ctx os m ()

  -- | Construct primitive array for mesh data
  newMeshDataVertexArray :: Proxy a -> BufferOf os a -> Render os (VertexArray t (ArrayOf a))

instance MeshBufferExt () where
  type BufferOf os () = Buffer os ()
  type ArrayOf () = ()
  newMeshDataBuffer _ n _ = newBuffer n
  {-# INLINE newMeshDataBuffer #-}
  writeMeshDataBuffer b p = writeBuffer b p . V.toList
  {-# INLINE writeMeshDataBuffer #-}
  newMeshDataVertexArray _ = newVertexArray
  {-# INLINE newMeshDataVertexArray #-}

instance MeshBufferExt Int32 where
  type BufferOf os Int32 = Buffer os (B Int32)
  type ArrayOf Int32 = B Int32
  newMeshDataBuffer _ n _ = newBuffer n
  {-# INLINE newMeshDataBuffer #-}
  writeMeshDataBuffer b p = writeBuffer b p . V.toList
  {-# INLINE writeMeshDataBuffer #-}
  newMeshDataVertexArray _ = newVertexArray
  {-# INLINE newMeshDataVertexArray #-}

instance MeshBufferExt (V3 Float) where
  type BufferOf os (V3 Float) = Buffer os (B3 Float)
  type ArrayOf (V3 Float) = B3 Float
  newMeshDataBuffer _ n _ = newBuffer n
  {-# INLINE newMeshDataBuffer #-}
  writeMeshDataBuffer b p = writeBuffer b p . V.toList
  {-# INLINE writeMeshDataBuffer #-}
  newMeshDataVertexArray _ = newVertexArray
  {-# INLINE newMeshDataVertexArray #-}

-- | Create new buffers with given capacity (count of vertecies and triangles)
newMeshBuffers :: (MonadIO m, ContextHandler ctx, MeshBufferExt a)
  => Proxy a
  -> Int -- ^ Count of vertecies
  -> Int -- ^ Count of triangles
  -> ContextT ctx os m (MeshBuffers os (BufferOf os a))
newMeshBuffers prox nverts ntrigs = MeshBuffers
  <$> newBuffer nverts
  <*> newBuffer nverts
  <*> newBuffer nverts
  <*> newBuffer (ntrigs * 3)
  <*> newMeshDataBuffer prox nverts ntrigs

-- | Write contents of mesh to the given buffer. Note that size of buffer should
-- match the mesh.
writeMeshBuffers :: (MonadIO m, ContextHandler ctx, MeshBufferExt a)
  => Mesh a
  -> MeshBuffers os (BufferOf os a)
  -> ContextT ctx os m ()
writeMeshBuffers M.Mesh{..} MeshBuffers{..} = do
  guardedWrite "positions" meshBuffPositions meshVertices
  guardedWrite "normals" meshBuffNormals meshNormals
  guardedWrite "uvs" meshBuffUvs meshUvs
  guardedWrite "indicies" meshBuffIndecies meshIndecies
  writeMeshDataBuffer meshBuffData 0 meshData
  where 
    guardedWrite name buff vec = do
      unless (bufferLength buff == V.length vec) $ 
        fail $ name ++ " buffer size " ++ 
          show (bufferLength buff) ++ 
          ", but expected " ++ (show $ V.length vec)
      writeBuffer buff 0 $ V.toList vec

-- | Create new buffers and fill it with vertecies from mesh
meshBuffers :: forall a m ctx os . (MonadIO m, ContextHandler ctx, MeshBufferExt a)
  => Mesh a
  -> ContextT ctx os m (MeshBuffers os (BufferOf os a))
meshBuffers m = do
  when (M.vertexNumber m == 0 || M.triangleNumber m == 0) $ 
    fail "Cannot fill mesh buffers with no vertecies!"
  bs <- newMeshBuffers (Proxy :: Proxy a) (M.vertexNumber m) (M.triangleNumber m)
  writeMeshBuffers m bs
  pure bs

-- | Convert mesh buffers to mesh vertex arrays
meshBufferArray :: forall os a p . MeshBufferExt a => Proxy a -> PrimitiveTopology p -> MeshBuffers os (BufferOf os a) -> Render os (PrimitiveArray p (MeshArray (ArrayOf a)))
meshBufferArray prox top MeshBuffers{..} = do
  p :: VertexArray () (B3 Float) <- newVertexArray meshBuffPositions
  n :: VertexArray () (B3 Float) <- newVertexArray meshBuffNormals
  u :: VertexArray () (B2 Float) <- newVertexArray meshBuffUvs
  a :: VertexArray () (ArrayOf a) <- newMeshDataVertexArray prox meshBuffData
  i :: IndexArray <- newIndexArray meshBuffIndecies Nothing
  pure $ toPrimitiveArrayIndexed top i $ zipVertices ($) (zipVertices ($) (zipVertices MeshArray p n) u) a
