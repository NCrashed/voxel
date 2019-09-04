-- | Conversion uboxed voxel grid to polygons (triangles)
module Data.Voxel.Grid.Unbox.Polygon(
    triangulate
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
import Linear (V2(..), V3(..))

import qualified Data.Vector.Storable.Mutable as VM
import qualified Data.Voxel.Grid.Unbox.Internal as G
import qualified Data.Voxel.Mesh as M
import qualified Data.Voxel.Mesh.Mutable as MM

-- | Convert each voxel to cubes build from triangles and return them. Usually
-- 'a' parameter used for color passing.
triangulate :: (Storable a, Unbox a, EmptyVoxel a, OpaqueVoxel a, Eq a) => VoxelGrid a -> Mesh a
triangulate g = negX <> posX <> negY <> posY <> negZ <> posZ
  where
    n = G.size g
    negX = foldMap (triangulateX g SideBackward) [0 .. n-1]
    posX = foldMap (triangulateX g SideForward) [0 .. n-1]
    negY = foldMap (triangulateY g SideLeft) [0 .. n-1]
    posY = foldMap (triangulateY g SideRight) [0 .. n-1]
    negZ = foldMap (triangulateZ g SideDown) [0 .. n-1]
    posZ = foldMap (triangulateZ g SideUp) [0 .. n-1]

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
  m <- VM.new (n*n)
  traverse_ (go m) [ V2 y z | y <- [0 .. n-1], z <- [0 .. n-1] ]
  pure m
  where
    n = G.size g
    go m i@(V2 y z) = setMask m n i $ g G.! (V3 x y z)

-- | Fill mask for Y plane
makeMaskY :: (Storable a, Unbox a) => VoxelGrid a -> Int -> ST s (Mask s a)
makeMaskY g y = do
  m <- VM.new (n*n)
  traverse_ (go m) [ V2 x z | x <- [0 .. n-1], z <- [0 .. n-1] ]
  pure m
  where
    n = G.size g
    go m i@(V2 x z) = setMask m n i $ g G.! (V3 x y z)

-- | Fill mask for Z plane
makeMaskZ :: (Storable a, Unbox a) => VoxelGrid a -> Int -> ST s (Mask s a)
makeMaskZ g z = do
  m <- VM.new (n*n)
  traverse_ (go m) [ V2 x y | x <- [0 .. n-1], y <- [0 .. n-1] ]
  pure m
  where
    n = G.size g
    go m i@(V2 x y) = setMask m n i $ g G.! (V3 x y z)

triangulateX :: (Storable a, Unbox a, EmptyVoxel a, OpaqueVoxel a, Eq a) => VoxelGrid a -> Side -> Int -> Mesh a
triangulateX g side x = runST $ do
  mask <- makeMaskX g x
  ms <- traverse (findQuad mask) [ V2 y z | y <- [0 .. n-1], z <- [0 .. n-1] ]
  pure $ mconcat . catMaybes $ ms
  where
    n = G.size g
    peekQuad mask (V2 y z) a = let
      step !i !acc@(V2 w h)
        | z + h >= n = pure acc
        | y + i >= n = step 0 (V2 i (h+1))
        | i >= w && h > 0 = step 0 (V2 w (h+1))
        | otherwise = do
          let y' = y + i
              z' = z + h
          am <- getMask mask n (V2 y' z')
          let visible = G.isVoxelSideVisible g (V3 x y' z') side
          if isEmptyVoxel am || am /= a || not visible then if i == 0
              then pure acc
              else step 0 (V2 i (h+1))
            else step (i+1) acc
      in step 1 (V2 0 0)
    findQuad mask i@(V2 y z) = do
      let j = V3 x y z
      let a = g G.! j
      am <- getMask mask n i
      if isEmptyVoxel am || not (G.isVoxelSideVisible g j side) then pure Nothing
      else do
        s <- peekQuad mask i a
        pure $ Just $ (if side == SideBackward then quadMeshNegX else quadMeshPosX) n (V3 x y z) s a

-- | Create mesh from single quad for -X plane
quadMeshNegX :: Storable a => Int -> V3 Int -> V2 Int -> a -> Mesh a
quadMeshNegX n (V3 x y z) (V2 w h) a = M.create $ do
  mm <- MM.new 4 2
  MM.writeVertex mm 0 p0 normal u0 a
  MM.writeVertex mm 1 p1 normal u1 a
  MM.writeVertex mm 2 p2 normal u2 a
  MM.writeVertex mm 3 p3 normal u3 a
  MM.writeIndex mm 0 $ V3 0 1 3
  MM.writeIndex mm 1 $ V3 1 2 3
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
quadMeshPosX :: Storable a => Int -> V3 Int -> V2 Int -> a -> Mesh a
quadMeshPosX n (V3 x y z) (V2 w h) a = M.create $ do
  mm <- MM.new 4 2
  MM.writeVertex mm 0 p0 normal u0 a
  MM.writeVertex mm 1 p1 normal u1 a
  MM.writeVertex mm 2 p2 normal u2 a
  MM.writeVertex mm 3 p3 normal u3 a
  MM.writeIndex mm 0 $ V3 3 1 0
  MM.writeIndex mm 1 $ V3 3 2 1
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

triangulateY :: (Storable a, Unbox a, EmptyVoxel a, OpaqueVoxel a, Eq a) => VoxelGrid a -> Side -> Int -> Mesh a
triangulateY g side y = runST $ do
  mask <- makeMaskY g y
  ms <- traverse (findQuad mask) [ V2 x z | x <- [0 .. n-1], z <- [0 .. n-1] ]
  pure $ mconcat . catMaybes $ ms
  where
    n = G.size g
    peekQuad mask (V2 x z) a = let
      step !i !acc@(V2 w h)
        | z + h >= n = pure acc
        | x + i >= n = step 0 (V2 i (h+1))
        | i >= w && h > 0 = step 0 (V2 w (h+1))
        | otherwise = do
          let x' = x + i
              z' = z + h
          am <- getMask mask n (V2 x' z')
          let visible = G.isVoxelSideVisible g (V3 x' y z') side
          if isEmptyVoxel am || am /= a || not visible then if i == 0
              then pure acc
              else step 0 (V2 i (h+1))
            else step (i+1) acc
      in step 1 (V2 0 0)
    findQuad mask i@(V2 x z) = do
      let j = V3 x y z
      let a = g G.! j
      am <- getMask mask n i
      if isEmptyVoxel am || not (G.isVoxelSideVisible g j side) then pure Nothing
      else do
        s <- peekQuad mask i a
        pure $ Just $ (if side == SideLeft then quadMeshNegY else quadMeshPosY) n (V3 x y z) s a

-- | Create mesh from single quad for -Y plane
quadMeshNegY :: Storable a => Int -> V3 Int -> V2 Int -> a -> Mesh a
quadMeshNegY n (V3 x y z) (V2 w h) a = M.create $ do
  mm <- MM.new 4 2
  MM.writeVertex mm 0 p0 normal u0 a
  MM.writeVertex mm 1 p1 normal u1 a
  MM.writeVertex mm 2 p2 normal u2 a
  MM.writeVertex mm 3 p3 normal u3 a
  MM.writeIndex mm 0 $ V3 0 1 3
  MM.writeIndex mm 1 $ V3 1 2 3
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
quadMeshPosY :: Storable a => Int -> V3 Int -> V2 Int -> a -> Mesh a
quadMeshPosY n (V3 x y z) (V2 w h) a = M.create $ do
  mm <- MM.new 4 2
  MM.writeVertex mm 0 p0 normal u0 a
  MM.writeVertex mm 1 p1 normal u1 a
  MM.writeVertex mm 2 p2 normal u2 a
  MM.writeVertex mm 3 p3 normal u3 a
  MM.writeIndex mm 0 $ V3 3 1 0
  MM.writeIndex mm 1 $ V3 3 2 1
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

triangulateZ :: (Storable a, Unbox a, EmptyVoxel a, OpaqueVoxel a, Eq a) => VoxelGrid a -> Side -> Int -> Mesh a
triangulateZ g side z = runST $ do
  mask <- makeMaskZ g z
  ms <- traverse (findQuad mask) [ V2 x y | x <- [0 .. n-1], y <- [0 .. n-1] ]
  pure $ mconcat . catMaybes $ ms
  where
    n = G.size g
    peekQuad mask (V2 x y) a = let
      step !i !acc@(V2 w h)
        | y + h >= n = pure acc
        | x + i >= n = step 0 (V2 i (h+1))
        | i >= w && h > 0 = step 0 (V2 w (h+1))
        | otherwise = do
          let x' = x + i
              y' = y + h
          am <- getMask mask n (V2 x' y')
          let visible = G.isVoxelSideVisible g (V3 x' y' z) side
          if isEmptyVoxel am || am /= a || not visible then if i == 0
              then pure acc
              else step 0 (V2 i (h+1))
            else step (i+1) acc
      in step 1 (V2 0 0)
    findQuad mask i@(V2 x y) = do
      let j = V3 x y z
      let a = g G.! j
      am <- getMask mask n i
      if isEmptyVoxel am || not (G.isVoxelSideVisible g j side) then pure Nothing
      else do
        s <- peekQuad mask i a
        pure $ Just $ (if side == SideDown then quadMeshNegZ else quadMeshPosZ) n (V3 x y z) s a


-- | Create mesh from single quad for -Z plane
quadMeshNegZ :: Storable a => Int -> V3 Int -> V2 Int -> a -> Mesh a
quadMeshNegZ n (V3 x y z) (V2 w h) a = M.create $ do
  mm <- MM.new 4 2
  MM.writeVertex mm 0 p0 normal u0 a
  MM.writeVertex mm 1 p1 normal u1 a
  MM.writeVertex mm 2 p2 normal u2 a
  MM.writeVertex mm 3 p3 normal u3 a
  MM.writeIndex mm 0 $ V3 0 1 3
  MM.writeIndex mm 1 $ V3 1 2 3
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
quadMeshPosZ :: Storable a => Int -> V3 Int -> V2 Int -> a -> Mesh a
quadMeshPosZ n (V3 x y z) (V2 w h) a = M.create $ do
  mm <- MM.new 4 2
  MM.writeVertex mm 0 p0 normal u0 a
  MM.writeVertex mm 1 p1 normal u1 a
  MM.writeVertex mm 2 p2 normal u2 a
  MM.writeVertex mm 3 p3 normal u3 a
  MM.writeIndex mm 0 $ V3 3 1 0
  MM.writeIndex mm 1 $ V3 3 2 1
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
