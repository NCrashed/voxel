-- | Conversion uboxed voxel grid to polygons (triangles)
module Data.Voxel.Grid.Unbox.Polygon(
    TriangulateTopology(..)
  , triangulate
  ) where

import Control.Monad.ST (runST, ST)
import Data.Foldable (traverse_)
import Data.Maybe
import Data.Vector.Unboxed (Unbox)
import Data.Voxel.Empty
import Data.Voxel.Grid.Unbox.Internal (VoxelGrid)
import Data.Voxel.Mesh (Mesh, Storable)
import Data.Voxel.Opaque
import Data.Voxel.Side
import GHC.Generics (Generic)
import Linear (V2(..), V3(..))

import qualified Data.Vector.Storable.Mutable as VM
import qualified Data.Voxel.Grid.Unbox.Internal as G
import qualified Data.Voxel.Mesh as M
import qualified Data.Voxel.Mesh.Mutable as MM

-- | Possible triangulation topolgies
data TriangulateTopology = TriangulateLines | TriangulateTriangles
  deriving (Eq, Ord, Enum, Bounded, Read, Show, Generic)

-- | Convert each voxel to cubes build from triangles and return them. Usually
-- 'a' parameter used for color passing.
triangulate :: (Storable a, Unbox a, EmptyVoxel a, OpaqueVoxel a, Eq a) => TriangulateTopology -> VoxelGrid a -> Mesh a
triangulate t g = negX <> posX <> negY <> posY <> negZ <> posZ
  where
    V3 nx ny nz = G.size g
    negX = foldMap (triangulateX t g SideBackward) [0 .. nx-1]
    posX = foldMap (triangulateX t g SideForward) [0 .. nx-1]
    negY = foldMap (triangulateY t g SideLeft) [0 .. ny-1]
    posY = foldMap (triangulateY t g SideRight) [0 .. ny-1]
    negZ = foldMap (triangulateZ t g SideDown) [0 .. nz-1]
    posZ = foldMap (triangulateZ t g SideUp) [0 .. nz-1]

-- | Mutable 2D vector that stores mask for visited voxels
type Mask s a = VM.STVector s a

setMask :: Storable a => Mask s a -> Int -> V2 Int -> a -> ST s ()
setMask m n (V2 x y) = VM.write m (x + y * n)
{-# INLINE setMask #-}

getMask :: Storable a => Mask s a -> Int -> V2 Int -> ST s a
getMask m n (V2 x y) = VM.read m (x + y * n)
{-# INLINE getMask #-}

-- | Fill mask for X plane
makeMaskX :: (Storable a, Unbox a) => VoxelGrid a -> Int -> ST s (Mask s a)
makeMaskX g x = do
  m <- VM.new (ny*nz)
  traverse_ (go m) [ V2 y z | y <- [0 .. ny-1], z <- [0 .. nz-1] ]
  pure m
  where
    V3 nx ny nz = G.size g
    go m i@(V2 y z) = setMask m ny i $ g G.! (V3 x y z)

-- | Fill mask for Y plane
makeMaskY :: (Storable a, Unbox a) => VoxelGrid a -> Int -> ST s (Mask s a)
makeMaskY g y = do
  m <- VM.new (nx*nz)
  traverse_ (go m) [ V2 x z | x <- [0 .. nx-1], z <- [0 .. nz-1] ]
  pure m
  where
    V3 nx ny nz = G.size g
    go m i@(V2 x z) = setMask m nx i $ g G.! (V3 x y z)

-- | Fill mask for Z plane
makeMaskZ :: (Storable a, Unbox a) => VoxelGrid a -> Int -> ST s (Mask s a)
makeMaskZ g z = do
  m <- VM.new (nx*ny)
  traverse_ (go m) [ V2 x y | x <- [0 .. nx-1], y <- [0 .. ny-1] ]
  pure m
  where
    V3 nx ny nz = G.size g
    go m i@(V2 x y) = setMask m nx i $ g G.! (V3 x y z)

triangulateX :: (Storable a, Unbox a, EmptyVoxel a, OpaqueVoxel a, Eq a) => TriangulateTopology -> VoxelGrid a -> Side -> Int -> Mesh a
triangulateX t g side x = runST $ do
  mask <- makeMaskX g x
  ms <- traverse (findQuad mask) [ V2 y z | y <- [0 .. ny-1], z <- [0 .. nz-1] ]
  pure $ mconcat . catMaybes $ ms
  where
    V3 nx ny nz = G.size g
    peekQuad mask (V2 y z) a = let
      step !i !acc@(V2 w h)
        | z + h >= nz = pure acc
        | y + i >= ny = step 0 (V2 i (h+1))
        | i >= w && h > 0 = step 0 (V2 w (h+1))
        | otherwise = do
          let y' = y + i
              z' = z + h
          am <- getMask mask ny (V2 y' z')
          let zeroMask = setMask mask ny (V2 y' z') emptyVoxel
          let visible = G.isVoxelSideVisible g (V3 x y' z') side
          if isEmptyVoxel am || am /= a || not visible then if i == 0
              then pure acc
              else step 0 (V2 i (h+1))
            else zeroMask >> step (i+1) acc
      in step 1 (V2 0 0)
    findQuad mask i@(V2 y z) = do
      let j = V3 x y z
      let a = g G.! j
      am <- getMask mask ny i
      if isEmptyVoxel am || not (G.isVoxelSideVisible g j side) then pure Nothing
      else do
        s <- peekQuad mask i a
        pure $ Just $ (if side == SideBackward then quadMeshNegX else quadMeshPosX) t nx (V3 x y z) s a

-- | Create mesh from single quad for -X plane
quadMeshNegX :: Storable a => TriangulateTopology -> Int -> V3 Int -> V2 Int -> a -> Mesh a
quadMeshNegX t n (V3 x y z) (V2 w h) a = M.create $ do
  mm <- case t of
    TriangulateTriangles -> MM.new 4 2
    TriangulateLines -> MM.new 4 6
  MM.writeVertex mm 0 p0 normal u0 a
  MM.writeVertex mm 1 p1 normal u1 a
  MM.writeVertex mm 2 p2 normal u2 a
  MM.writeVertex mm 3 p3 normal u3 a
  case t of
    TriangulateTriangles -> do
      MM.writeTriangleIndex mm 0 $ V3 0 1 3
      MM.writeTriangleIndex mm 1 $ V3 1 2 3
    TriangulateLines -> do
      MM.writeLineIndex mm 0 $ V2 0 1
      MM.writeLineIndex mm 1 $ V2 1 3
      MM.writeLineIndex mm 2 $ V2 3 0
      MM.writeLineIndex mm 3 $ V2 1 2
      MM.writeLineIndex mm 4 $ V2 2 3
      MM.writeLineIndex mm 5 $ V2 3 1
  pure mm
  where
    normal = V3 (-1) 0 0
    p0 = V3 (norm x) (norm y) (norm z)
    p1 = V3 (norm x) (norm $ y + w) (norm z)
    p2 = V3 (norm x) (norm $ y + w) (norm $ z + h)
    p3 = V3 (norm x) (norm y) (norm $ z + h)
    u0 = V2 0 (fromIntegral h)
    u1 = V2 (fromIntegral w) (fromIntegral h)
    u2 = V2 (fromIntegral w) 0
    u3 = V2 0 0
    norm v = fromIntegral v / fromIntegral n

-- | Create mesh from single quad for +X plane
quadMeshPosX :: Storable a => TriangulateTopology -> Int -> V3 Int -> V2 Int -> a -> Mesh a
quadMeshPosX t n (V3 x y z) (V2 w h) a = M.create $ do
  mm <- case t of
    TriangulateTriangles -> MM.new 4 2
    TriangulateLines -> MM.new 4 6
  MM.writeVertex mm 0 p0 normal u0 a
  MM.writeVertex mm 1 p1 normal u1 a
  MM.writeVertex mm 2 p2 normal u2 a
  MM.writeVertex mm 3 p3 normal u3 a
  case t of
    TriangulateTriangles -> do
      MM.writeTriangleIndex mm 0 $ V3 3 1 0
      MM.writeTriangleIndex mm 1 $ V3 3 2 1
    TriangulateLines -> do
      MM.writeLineIndex mm 0 $ V2 0 1
      MM.writeLineIndex mm 1 $ V2 1 3
      MM.writeLineIndex mm 2 $ V2 3 0
      MM.writeLineIndex mm 3 $ V2 1 2
      MM.writeLineIndex mm 4 $ V2 2 3
      MM.writeLineIndex mm 5 $ V2 3 1
  pure mm
  where
    normal = V3 1 0 0
    p0 = V3 (norm $ x+1) (norm y) (norm z)
    p1 = V3 (norm $ x+1) (norm $ y + w) (norm z)
    p2 = V3 (norm $ x+1) (norm $ y + w) (norm $ z + h)
    p3 = V3 (norm $ x+1) (norm y) (norm $ z + h)
    u0 = V2 0 (fromIntegral h)
    u1 = V2 (fromIntegral w) (fromIntegral h)
    u2 = V2 (fromIntegral w) 0
    u3 = V2 0 0
    norm v = fromIntegral v / fromIntegral n

triangulateY :: (Storable a, Unbox a, EmptyVoxel a, OpaqueVoxel a, Eq a) => TriangulateTopology -> VoxelGrid a -> Side -> Int -> Mesh a
triangulateY t g side y = runST $ do
  mask <- makeMaskY g y
  ms <- traverse (findQuad mask) [ V2 x z | x <- [0 .. nx-1], z <- [0 .. nz-1] ]
  pure $ mconcat . catMaybes $ ms
  where
    V3 nx ny nz = G.size g
    peekQuad mask (V2 x z) a = let
      step !i !acc@(V2 w h)
        | z + h >= nz = pure acc
        | x + i >= nx = step 0 (V2 i (h+1))
        | i >= w && h > 0 = step 0 (V2 w (h+1))
        | otherwise = do
          let x' = x + i
              z' = z + h
          am <- getMask mask nx (V2 x' z')
          let zeroMask = setMask mask nx (V2 x' z') emptyVoxel
          let visible = G.isVoxelSideVisible g (V3 x' y z') side
          if isEmptyVoxel am || am /= a || not visible then if i == 0
              then pure acc
              else step 0 (V2 i (h+1))
            else zeroMask >> step (i+1) acc
      in step 1 (V2 0 0)
    findQuad mask i@(V2 x z) = do
      let j = V3 x y z
      let a = g G.! j
      am <- getMask mask nx i
      if isEmptyVoxel am || not (G.isVoxelSideVisible g j side) then pure Nothing
      else do
        s <- peekQuad mask i a
        pure $ Just $ (if side == SideLeft then quadMeshNegY else quadMeshPosY) t ny (V3 x y z) s a

-- | Create mesh from single quad for -Y plane
quadMeshNegY :: Storable a =>  TriangulateTopology -> Int -> V3 Int -> V2 Int -> a -> Mesh a
quadMeshNegY t n (V3 x y z) (V2 w h) a = M.create $ do
  mm <- case t of
    TriangulateTriangles -> MM.new 4 2
    TriangulateLines -> MM.new 4 6
  MM.writeVertex mm 0 p0 normal u0 a
  MM.writeVertex mm 1 p1 normal u1 a
  MM.writeVertex mm 2 p2 normal u2 a
  MM.writeVertex mm 3 p3 normal u3 a
  case t of
    TriangulateTriangles -> do
      MM.writeTriangleIndex mm 0 $ V3 0 1 3
      MM.writeTriangleIndex mm 1 $ V3 1 2 3
    TriangulateLines -> do
      MM.writeLineIndex mm 0 $ V2 0 1
      MM.writeLineIndex mm 1 $ V2 1 3
      MM.writeLineIndex mm 2 $ V2 3 0
      MM.writeLineIndex mm 3 $ V2 1 2
      MM.writeLineIndex mm 4 $ V2 2 3
      MM.writeLineIndex mm 5 $ V2 3 1
  pure mm
  where
    normal = V3 0 (-1) 0
    p0 = V3 (norm x)       (norm y) (norm z)
    p1 = V3 (norm $ x + w) (norm y) (norm z)
    p2 = V3 (norm $ x + w) (norm y) (norm $ z + h)
    p3 = V3 (norm x)       (norm y) (norm $ z + h)
    u0 = V2 0 (fromIntegral h)
    u1 = V2 (fromIntegral w) (fromIntegral h)
    u2 = V2 (fromIntegral w) 0
    u3 = V2 0 0
    norm v = fromIntegral v / fromIntegral n

-- | Create mesh from single quad for +Y plane
quadMeshPosY :: Storable a => TriangulateTopology -> Int -> V3 Int -> V2 Int -> a -> Mesh a
quadMeshPosY t n (V3 x y z) (V2 w h) a = M.create $ do
  mm <- case t of
    TriangulateTriangles -> MM.new 4 2
    TriangulateLines -> MM.new 4 6
  MM.writeVertex mm 0 p0 normal u0 a
  MM.writeVertex mm 1 p1 normal u1 a
  MM.writeVertex mm 2 p2 normal u2 a
  MM.writeVertex mm 3 p3 normal u3 a
  case t of
    TriangulateTriangles -> do
      MM.writeTriangleIndex mm 0 $ V3 3 1 0
      MM.writeTriangleIndex mm 1 $ V3 3 2 1
    TriangulateLines -> do
      MM.writeLineIndex mm 0 $ V2 0 1
      MM.writeLineIndex mm 1 $ V2 1 3
      MM.writeLineIndex mm 2 $ V2 3 0
      MM.writeLineIndex mm 3 $ V2 1 2
      MM.writeLineIndex mm 4 $ V2 2 3
      MM.writeLineIndex mm 5 $ V2 3 1
  pure mm
  where
    normal = V3 0 1 0
    p0 = V3 (norm x)     (norm $ y+1) (norm z)
    p1 = V3 (norm $ x+w) (norm $ y+1) (norm z)
    p2 = V3 (norm $ x+w) (norm $ y+1) (norm $ z + h)
    p3 = V3 (norm x)     (norm $ y+1) (norm $ z + h)
    u0 = V2 0 (fromIntegral h)
    u1 = V2 (fromIntegral w) (fromIntegral h)
    u2 = V2 (fromIntegral w) 0
    u3 = V2 0 0
    norm v = fromIntegral v / fromIntegral n

triangulateZ :: (Storable a, Unbox a, EmptyVoxel a, OpaqueVoxel a, Eq a) => TriangulateTopology -> VoxelGrid a -> Side -> Int -> Mesh a
triangulateZ t g side z = runST $ do
  mask <- makeMaskZ g z
  ms <- traverse (findQuad mask) [ V2 x y | x <- [0 .. nx-1], y <- [0 .. ny-1] ]
  pure $ mconcat . catMaybes $ ms
  where
    V3 nx ny nz = G.size g
    peekQuad mask (V2 x y) a = let
      step !i !acc@(V2 w h)
        | y + h >= ny = pure acc
        | x + i >= nx = step 0 (V2 i (h+1))
        | i >= w && h > 0 = step 0 (V2 w (h+1))
        | otherwise = do
          let x' = x + i
              y' = y + h
          am <- getMask mask nx (V2 x' y')
          let zeroMask = setMask mask nx (V2 x' y') emptyVoxel
          let visible = G.isVoxelSideVisible g (V3 x' y' z) side
          if isEmptyVoxel am || am /= a || not visible then if i == 0
              then pure acc
              else step 0 (V2 i (h+1))
            else zeroMask >> step (i+1) acc
      in step 1 (V2 0 0)
    findQuad mask i@(V2 x y) = do
      let j = V3 x y z
      let a = g G.! j
      am <- getMask mask nx i
      if isEmptyVoxel am || not (G.isVoxelSideVisible g j side) then pure Nothing
      else do
        s <- peekQuad mask i a
        pure $ Just $ (if side == SideDown then quadMeshNegZ else quadMeshPosZ) t nz (V3 x y z) s a


-- | Create mesh from single quad for -Z plane
quadMeshNegZ :: Storable a => TriangulateTopology -> Int -> V3 Int -> V2 Int -> a -> Mesh a
quadMeshNegZ t n (V3 x y z) (V2 w h) a = M.create $ do
  mm <- case t of
    TriangulateTriangles -> MM.new 4 2
    TriangulateLines -> MM.new 4 6
  MM.writeVertex mm 0 p0 normal u0 a
  MM.writeVertex mm 1 p1 normal u1 a
  MM.writeVertex mm 2 p2 normal u2 a
  MM.writeVertex mm 3 p3 normal u3 a
  case t of
    TriangulateTriangles -> do
      MM.writeTriangleIndex mm 0 $ V3 0 1 3
      MM.writeTriangleIndex mm 1 $ V3 1 2 3
    TriangulateLines -> do
      MM.writeLineIndex mm 0 $ V2 0 1
      MM.writeLineIndex mm 1 $ V2 1 3
      MM.writeLineIndex mm 2 $ V2 3 0
      MM.writeLineIndex mm 3 $ V2 1 2
      MM.writeLineIndex mm 4 $ V2 2 3
      MM.writeLineIndex mm 5 $ V2 3 1
  pure mm
  where
    normal = V3 0 0 (-1)
    p0 = V3 (norm x)       (norm y)        (norm z)
    p1 = V3 (norm $ x + w) (norm y)        (norm z)
    p2 = V3 (norm $ x + w) (norm $ y + h)  (norm z)
    p3 = V3 (norm x)       (norm $ y  + h) (norm z)
    u0 = V2 0 (fromIntegral h)
    u1 = V2 (fromIntegral w) (fromIntegral h)
    u2 = V2 (fromIntegral w) 0
    u3 = V2 0 0
    norm v = fromIntegral v / fromIntegral n

-- | Create mesh from single quad for +Z plane
quadMeshPosZ :: Storable a => TriangulateTopology -> Int -> V3 Int -> V2 Int -> a -> Mesh a
quadMeshPosZ t n (V3 x y z) (V2 w h) a = M.create $ do
  mm <- case t of
    TriangulateTriangles -> MM.new 4 2
    TriangulateLines -> MM.new 4 6
  MM.writeVertex mm 0 p0 normal u0 a
  MM.writeVertex mm 1 p1 normal u1 a
  MM.writeVertex mm 2 p2 normal u2 a
  MM.writeVertex mm 3 p3 normal u3 a
  case t of
    TriangulateTriangles -> do
      MM.writeTriangleIndex mm 0 $ V3 3 1 0
      MM.writeTriangleIndex mm 1 $ V3 3 2 1
    TriangulateLines -> do
      MM.writeLineIndex mm 0 $ V2 0 1
      MM.writeLineIndex mm 1 $ V2 1 3
      MM.writeLineIndex mm 2 $ V2 3 0
      MM.writeLineIndex mm 3 $ V2 1 2
      MM.writeLineIndex mm 4 $ V2 2 3
      MM.writeLineIndex mm 5 $ V2 3 1
  pure mm
  where
    normal = V3 0 0 1
    p0 = V3 (norm x)     (norm $ y)     (norm $ z+1)
    p1 = V3 (norm $ x+w) (norm $ y)     (norm $ z+1)
    p2 = V3 (norm $ x+w) (norm $ y + h) (norm $ z+1)
    p3 = V3 (norm x)     (norm $ y + h) (norm $ z+1)
    u0 = V2 0 (fromIntegral h)
    u1 = V2 (fromIntegral w) (fromIntegral h)
    u2 = V2 (fromIntegral w) 0
    u3 = V2 0 0
    norm v = fromIntegral v / fromIntegral n
