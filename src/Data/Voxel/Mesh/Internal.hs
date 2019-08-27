module Data.Voxel.Mesh.Internal where

import Control.Monad.ST
import Data.Int
import Data.Vector.Storable (Vector, Storable)
import Data.Voxel.Mesh.Mutable.Internal (MMesh)
import Data.Word
import GHC.Generics
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
} deriving (Show, Generic)

-- | Get count of single vertecies in the mesh
vertexNumber :: Mesh a -> Int
vertexNumber Mesh{..} = V.length meshVertices
{-# INLINE vertexNumber #-}

-- | Get count of triangles in the mesh
triangleNumber :: Mesh a -> Int
triangleNumber Mesh{..} = V.length meshIndecies `div` 3
{-# INLINE triangleNumber #-}

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

-- | Testing mesh of cube of size 2 at center of coords
cube :: Mesh Int32
cube = Mesh {
    meshVertices = V.fromList . concat $ [ mkPoses p0 u l | (p0, u, l) <- faces]
  , meshNormals  = V.fromList . concat $ [ mkNormals u l | (_, u, l) <- faces]
  , meshUvs      = V.fromList . concat $ [ mkUvs | _ <- faces]
  , meshIndecies = V.fromList . concat $ [ mkIndecies (fromIntegral $ i * 4) | i <- [0 .. length faces - 1]]
  , meshData     = V.fromList . concat $ [ replicate 4 0 | _ <- faces]
  }
  where
    faces = [
            (V3 (-1) (-1) (-1), V3 2 0 0,  V3 0 2 0)
          , (V3 (-1) (-1) (-1), V3 0 2 0,  V3 0 0 2)
          , (V3 (-1) (-1) (-1), V3 0 0 2,  V3 2 0 0)
          , (V3 1 1 1, V3 0 (-2) 0, V3 (-2) 0 0)
          , (V3 1 1 1, V3 0 0 (-2), V3 0 (-2) 0)
          , (V3 1 1 1, V3 (-2) 0 0, V3 0 0 (-2))
          ]
    ntrigs = length faces * 2
    nverts = length faces * 4
    mkPoses p0 u l = [ p0, p0 + l, p0 + u, p0 + u + l ]
    mkNormals u l = let n = normalize $ cross l u in replicate 4 n
    mkUvs = [ V2 0 1, V2 1 1, V2 0 0, V2 1 0 ]
    mkIndecies o = [ o, o+1, o+2, o+1, o+2, o+3 ]
