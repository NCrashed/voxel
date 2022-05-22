module Data.Voxel.Grid.UnboxTest where

import Data.Foldable (traverse_)
import Linear
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.Util

import Data.Voxel.Grid.Unbox (VoxelGrid)
import qualified Data.Voxel.Grid.Unbox.Internal as G
import qualified Data.Voxel.Grid.Unbox.Mutable.Internal as M

unit_unbox_empty :: IO ()
unit_unbox_empty = do
  let g :: VoxelGrid Int = G.empty 3
  assertEqual "size" 3 $ G.size g
  assertEqual "size" 3 $ G.size g

unit_unbox_create :: IO ()
unit_unbox_create = sequence_ [single i | i <- [1 .. 10]]
  where
    single n = do
      let g :: VoxelGrid Int = G.create $ M.replicate n 42
      assertEqual ("Grid of size " <> show n) (G.toList g) $ replicate (n*n*n) 42
