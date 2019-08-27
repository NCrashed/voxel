{-# LANGUAGE Arrows #-}
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
import Control.Monad.IO.Class
import Data.Int
import Data.Proxy
import Data.Vector.Storable (Vector, Storable)
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
  type BufferOf os a :: *
  -- | Maps from mesh data element to primitive array element
  type ArrayOf a :: *

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
  writeBuffer meshBuffPositions 0 $ V.toList meshVertices
  writeBuffer meshBuffNormals 0 $ V.toList meshNormals
  writeBuffer meshBuffUvs 0 $ V.toList meshUvs
  writeBuffer meshBuffIndecies 0 $ V.toList meshIndecies
  writeMeshDataBuffer meshBuffData 0 meshData

-- | Create new buffers and fill it with vertecies from mesh
meshBuffers :: forall a m ctx os . (MonadIO m, ContextHandler ctx, MeshBufferExt a)
  => Mesh a
  -> ContextT ctx os m (MeshBuffers os (BufferOf os a))
meshBuffers m = do
  bs <- newMeshBuffers (Proxy :: Proxy a) (M.vertexNumber m) (M.triangleNumber m)
  writeMeshBuffers m bs
  pure bs

-- | Convert mesh buffers to mesh vertex arrays
meshBufferArray :: forall os a . MeshBufferExt a => Proxy a -> MeshBuffers os (BufferOf os a) -> Render os (PrimitiveArray Triangles (MeshArray (ArrayOf a)))
meshBufferArray prox MeshBuffers{..} = do
  p :: VertexArray () (B3 Float) <- newVertexArray meshBuffPositions
  n :: VertexArray () (B3 Float) <- newVertexArray meshBuffNormals
  u :: VertexArray () (B2 Float) <- newVertexArray meshBuffUvs
  a :: VertexArray () (ArrayOf a) <- newMeshDataVertexArray prox meshBuffData
  i :: IndexArray <- newIndexArray meshBuffIndecies Nothing
  pure $ toPrimitiveArrayIndexed TriangleList i $ zipVertices ($) (zipVertices ($) (zipVertices MeshArray p n) u) a
