module Data.Voxel.Grid.Unbox.MutableTest where

import Data.Foldable (traverse_)
import Linear
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.Util

import Data.Voxel.Grid.Unbox.Mutable (MVoxelGrid, IOVoxelGrid)
import qualified Data.Voxel.Grid.Unbox.Mutable.Internal as GM

unit_mutable_newSizes :: IO ()
unit_mutable_newSizes = traverse_ single [0, 1, 3, 16, 32, 64, 128]
  where
    single n = do
      g :: IOVoxelGrid Int <- GM.new (V3 n n n)
      assertEqual "length" (n*n*n) $ GM.length g
      assertEqual "size" (V3 n n n) $ GM.size g

prop_indeciesIdenmponent :: V3 (NonNegative Int) -> Int -> Bool
prop_indeciesIdenmponent n i
  | nx == 0 || ny == 0 || nz == 0 = True
  | otherwise = GM.posToIndex n' (GM.indexToPos n' i) == i
  where 
    n' = V3 nx ny nz
    (V3 (NonNegative nx) (NonNegative ny) (NonNegative nz)) = n

unit_mutable_indecies :: IO ()
unit_mutable_indecies = do
  assertEqual "index 3 {0;0;0}" 0 $ GM.posToIndex 3 (V3 0 0 0)
  assertEqual "index 3 {1;0;0}" 1 $ GM.posToIndex 3 (V3 1 0 0)
  assertEqual "index 3 {2;0;0}" 2 $ GM.posToIndex 3 (V3 2 0 0)
  assertEqual "index 3 {0;1;0}" 3 $ GM.posToIndex 3 (V3 0 1 0)
  assertEqual "index 3 {1;1;0}" 4 $ GM.posToIndex 3 (V3 1 1 0)
  assertEqual "index 3 {2;1;0}" 5 $ GM.posToIndex 3 (V3 2 1 0)
  assertEqual "index 3 {0;2;0}" 6 $ GM.posToIndex 3 (V3 0 2 0)
  assertEqual "index 3 {1;2;0}" 7 $ GM.posToIndex 3 (V3 1 2 0)
  assertEqual "index 3 {2;2;0}" 8 $ GM.posToIndex 3 (V3 2 2 0)
  assertEqual "index 3 {0;0;1}" 9 $ GM.posToIndex 3 (V3 0 0 1)
  assertEqual "index 3 {1;0;1}" 10 $ GM.posToIndex 3 (V3 1 0 1)
  assertEqual "index 3 {2;0;1}" 11 $ GM.posToIndex 3 (V3 2 0 1)
  assertEqual "index 3 {0;1;1}" 12 $ GM.posToIndex 3 (V3 0 1 1)
  assertEqual "index 3 {1;1;1}" 13 $ GM.posToIndex 3 (V3 1 1 1)
  assertEqual "index 3 {2;1;1}" 14 $ GM.posToIndex 3 (V3 2 1 1)
  assertEqual "index 3 {0;2;1}" 15 $ GM.posToIndex 3 (V3 0 2 1)
  assertEqual "index 3 {1;2;1}" 16 $ GM.posToIndex 3 (V3 1 2 1)
  assertEqual "index 3 {2;2;1}" 17 $ GM.posToIndex 3 (V3 2 2 1)
  assertEqual "index 3 {0;0;2}" 18 $ GM.posToIndex 3 (V3 0 0 2)
  assertEqual "index 3 {1;0;2}" 19 $ GM.posToIndex 3 (V3 1 0 2)
  assertEqual "index 3 {2;0;2}" 20 $ GM.posToIndex 3 (V3 2 0 2)
  assertEqual "index 3 {0;1;2}" 21 $ GM.posToIndex 3 (V3 0 1 2)
  assertEqual "index 3 {1;1;2}" 22 $ GM.posToIndex 3 (V3 1 1 2)
  assertEqual "index 3 {2;1;2}" 23 $ GM.posToIndex 3 (V3 2 1 2)
  assertEqual "index 3 {0;2;2}" 24 $ GM.posToIndex 3 (V3 0 2 2)
  assertEqual "index 3 {1;2;2}" 25 $ GM.posToIndex 3 (V3 1 2 2)
  assertEqual "index 3 {2;2;2}" 26 $ GM.posToIndex 3 (V3 2 2 2)

unit_mutable_indeciesBounds :: IO ()
unit_mutable_indeciesBounds = do
  assertBool "in bounds 1 {0;0;0}" $ GM.posInBounds 1 (V3 0 0 0)
  assertBool "not in bounds (-1) {0;0;0}" $ not $ GM.posInBounds (-1) (V3 0 0 0)
  assertBool "not in bounds (-3) {-1;-1;-1}" $ not $ GM.posInBounds (-3) (V3 (-1) (-1) (-1))
  assertBool "in bounds 3 {0;0;0}" $ GM.posInBounds 3 (V3 0 0 0)
  assertBool "in bounds 3 {1;2;1}" $ GM.posInBounds 3 (V3 1 2 1)
  assertBool "in bounds 3 {2;2;2}" $ GM.posInBounds 3 (V3 2 2 2)
  assertBool "not in bounds 3 {-1;0;0}" $ not $ GM.posInBounds 3 (V3 (-1) 0 0)
  assertBool "not in bounds 3 {0;-1;0}" $ not $ GM.posInBounds 3 (V3 0 (-1) 0)
  assertBool "not in bounds 3 {0;0;-1}" $ not $ GM.posInBounds 3 (V3 0 0 (-1))
  assertBool "not in bounds 3 {3;3;3}" $ not $ GM.posInBounds 3 (V3 3 3 3)
  assertBool "not in bounds 3 {3;0;0}" $ not $ GM.posInBounds 3 (V3 3 0 0)
  assertBool "not in bounds 3 {0;3;0}" $ not $ GM.posInBounds 3 (V3 0 3 0)
  assertBool "not in bounds 3 {0;0;3}" $ not $ GM.posInBounds 3 (V3 0 0 3)
  assertBool "in bounds 10 {9;9;9}" $ GM.posInBounds 10 (V3 9 9 9)
  assertBool "in bounds max {max-1;max-1;max-1}" $ GM.posInBounds maxBound (V3 (maxBound-1) (maxBound-1) (maxBound-1))

unit_mutable_readWrite :: IO ()
unit_mutable_readWrite = traverse_ single [(s, GM.indexToPos s i, a)|
    s@(V3 sx sy sz) <- sizes
  , i <- [0 .. sx*sy*sz-1]
  , a <- values]
  where
    sizes = [V3 1 1 1, V3 3 3 3, V3 16 16 16, V3 32 32 32]
    values = [0, (-42), 42, maxBound, minBound]
    single (n,i,a) = do
      g :: IOVoxelGrid Int <- GM.new n
      GM.write g i a
      a' <- GM.read g i
      assertEqual "read write" a a'

unit_mutable_readWriteUnbound :: IO ()
unit_mutable_readWriteUnbound = do
  single 0 1
  single 1 (-1)
  single 1 1
  single (-1) 0
  single 3 (V3 3 0 0)
  single 3 (V3 0 3 0)
  single 3 (V3 0 0 3)
  where
    single n i = assertThrows $ do
      g :: IOVoxelGrid Int <- GM.new n
      let a = 42
      GM.write g i a
      a' <- GM.read g i
      assertEqual "read write" a a'

unit_mutable_swaps :: IO ()
unit_mutable_swaps = traverse_ single [(s, GM.indexToPos s i1, GM.indexToPos s i2, a1, a2)|
    s@(V3 sx sy sz) <- sizes
  , i1 <- [0 .. sx*sy*sz-1]
  , let i2 = sx*sy*sz - 1 - i1
  , i1 /= i2
  , a1 <- values
  , a2 <- reverse values]
  where
    sizes = [V3 1 1 1, V3 3 3 3, V3 16 16 16, V3 32 32 32]
    values = [0, (-42), 42, maxBound, minBound]
    single args@(n,i1,i2,a1,a2) = do
      g :: IOVoxelGrid Int <- GM.new n
      GM.write g i1 a1
      GM.write g i2 a2
      GM.swap g i1 i2
      a1' <- GM.read g i1
      a2' <- GM.read g i2
      assertEqual ("a1 for " ++ show args) a2 a1'
      assertEqual ("a2 for " ++ show args) a1 a2'

unit_mutable_unsafeReadWrite :: IO ()
unit_mutable_unsafeReadWrite = traverse_ single [(s, GM.indexToPos s i, a)|
    s@(V3 sx sy sz) <- sizes
  , i <- [0 .. sx*sy*sz-1]
  , a <- values]
  where
    sizes = [V3 1 1 1, V3 3 3 3, V3 16 16 16, V3 32 32 32]
    values = [0, (-42), 42, maxBound, minBound]
    single (n,i,a) = do
      g :: IOVoxelGrid Int <- GM.new n
      GM.unsafeWrite g i a
      a' <- GM.unsafeRead g i
      assertEqual "read write" a a'

unit_mutable_unsafeSwaps :: IO ()
unit_mutable_unsafeSwaps = traverse_ single [(s, GM.indexToPos s i1, GM.indexToPos s i2, a1, a2)|
    s@(V3 sx sy sz) <- sizes
  , i1 <- [0 .. sx*sy*sz-1]
  , let i2 = sx*sy*sz - 1 - i1
  , i1 /= i2
  , a1 <- values
  , a2 <- reverse values]
  where
    sizes = [V3 1 1 1, V3 3 3 3, V3 16 16 16, V3 32 32 32]
    values = [0, (-42), 42, maxBound, minBound]
    single args@(n,i1,i2,a1,a2) = do
      g :: IOVoxelGrid Int <- GM.new n
      GM.unsafeWrite g i1 a1
      GM.unsafeWrite g i2 a2
      GM.unsafeSwap g i1 i2
      a1' <- GM.unsafeRead g i1
      a2' <- GM.unsafeRead g i2
      assertEqual ("a1 for " ++ show args) a2 a1'
      assertEqual ("a2 for " ++ show args) a1 a2'
