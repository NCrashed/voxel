module Game.Player(
    playerModel
  ) where 

import Linear

import qualified Data.Voxel.Grid.Unbox as G

-- | Procedural player model as voxel grid
playerModel :: G.VoxelGrid (V3 Float)
playerModel = G.generate (V3 3 2 1) $ \(V3 x y _) -> if 
  | x == 0 && y == 0 -> V3 1.0 0.0 0.0
  | x == 1 && y == 1 -> V3 1.0 0.0 0.0
  | x == 2 && y == 0 -> V3 1.0 0.0 0.0
  | otherwise -> V3 0.0 0.0 0.0