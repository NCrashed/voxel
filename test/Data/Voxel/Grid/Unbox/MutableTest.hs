module Data.Voxel.Grid.Unbox.MutableTest where

import Data.Foldable (traverse_)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Data.Voxel.Grid.Unbox.Mutable (MVoxelGrid, IOVoxelGrid)
import qualified Data.Voxel.Grid.Unbox.Mutable.Internal as GM

unit_newSizes :: IO ()
unit_newSizes = traverse_ single [0, 1, 3, 16, 32, 64, 128]
  where
    single n = do
      g :: IOVoxelGrid Int <- GM.new n
      assertEqual "length" (n*n*n) $ GM.length g
      assertEqual "size" n $ GM.size g

prop_indeciesIdenmponent :: Int -> Int -> Bool
prop_indeciesIdenmponent n i
  | n == 0 = True
  | otherwise = GM.posToIndex n (GM.indexToPos n i) == i
