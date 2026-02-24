module Data.Voxel.Mesh.Lod.Internal where 

import Data.Bifunctor
import Data.Bits
import Data.Foldable (traverse_)
import Data.IntMap (IntMap)
import Data.Maybe
import Data.Vector.Unboxed (Unbox)
import Data.Voxel.Combine
import Data.Voxel.Empty
import Data.Voxel.Grid.Unbox (VoxelGrid)
import Data.Voxel.Grid.Unbox.Polygon
import Data.Voxel.Mesh (Mesh)
import Data.Voxel.Opaque
import Foreign.Storable
import GHC.Generics (Generic)
import Linear 

import qualified Data.IntMap as M 
import qualified Data.Voxel.Grid.Unbox as G 
import qualified Data.Voxel.Grid.Unbox.Mutable as GM
import qualified Data.Voxel.Mesh as VM

-- | Dynamically calculates LOD meshes for given grid
data LodMesh a = LodMesh {
    lodVoxels :: !(VoxelGrid a)
  , lodCount  :: !Int
  , lodLevels :: !(IntMap (Mesh a))
  } deriving (Show, Generic)

-- | Wrap voxel grid with LOD mesh
new :: (Storable a, Unbox a, EmptyVoxel a, OpaqueVoxel a, CombineVoxel a, Eq a) 
  => TriangulateTopology -> VoxelGrid a -> LodMesh a 
new topology grid = LodMesh grid (length lodsList) lods 
  where 
    n = gridLods grid
    keepNonEmpty = filter ((\a -> VM.triangleNumber a /= 0) . snd) 
    lodsList = reverse $ keepNonEmpty $ second (triangulate topology) <$> foldl' makeLod [(0, grid)] [1 .. n-1]
    lods = M.fromAscList lodsList
    makeLod ((!j, g):acc) i = (i, nextGridLod g) : (j, g) : acc
    makeLod _ _ = error "LodMesh new impossible"

-- | Calculate number of LOD levels for size of a grid
gridLods :: VoxelGrid a -> Int 
gridLods g = x `max` y `max` z
  where 
    V3 x y z = unpow 1 <$> G.size g
    unpow !i !v 
      | v <= 1 = i
      | otherwise = unpow (i+1) (v `shiftR` 1)

-- | Calculate LOD voxel grid from given voxels
nextGridLod :: (Unbox a, EmptyVoxel a, CombineVoxel a) 
  => VoxelGrid a -> VoxelGrid a 
nextGridLod g = G.create $ do 
  let lsize@(V3 xn yn zn) = (max 1) . (`shiftR` 1) <$> G.size g
  m <- GM.new lsize
  let is = [V3 x y z| x <- [0 .. xn-1], y <- [0 .. yn-1], z <- [0 .. zn-1]]
  let writeLodVoxel v@(V3 x y z) = do 
        let at i = fromMaybe emptyVoxel $ g G.!? i 
        let v0 = at $ V3 (2*x)   (2*y)   (2*z) 
        let v1 = at $ V3 (2*x+1) (2*y)   (2*z) 
        let v2 = at $ V3 (2*x)   (2*y+1) (2*z) 
        let v3 = at $ V3 (2*x+1) (2*y+1) (2*z) 
        let v4 = at $ V3 (2*x)   (2*y)   (2*z+1) 
        let v5 = at $ V3 (2*x+1) (2*y)   (2*z+1)  
        let v6 = at $ V3 (2*x)   (2*y+1) (2*z+1)  
        let v7 = at $ V3 (2*x+1) (2*y+1) (2*z+1)
        let value = combineCube v0 v1 v2 v3 v4 v5 v6 v7
        GM.unsafeWrite m v value
  traverse_ writeLodVoxel is 
  pure m

-- | Calculate size of voxel grid for given LOD level
gridLodSize :: Int -> VoxelGrid a -> V3 Int 
gridLodSize i g = unpow i $ G.size g 
    where 
      unpow !j !v
        | j <= 0 = v 
        | otherwise = unpow (j-1) $ (max 1) . (`shiftR` 1) <$> v

-- | Get mesh corresponding to the LOD level.
-- 0 means the most detailed and each next positive
-- number decreases details.
lod :: Int -> LodMesh a -> Mesh a 
lod i g 
  | i < 0 = lod 0 g
  | i >= lodCount g = lod (lodCount g) g 
  | otherwise = case M.lookup i (lodLevels g) of 
    Nothing -> error $ "lod: Impossible, not found lod level " ++ show i
    Just v -> v 

-- | Get all lods levels as a list
allLods :: LodMesh a -> [Mesh a]
allLods m = fmap (`lod` m) [0 .. lodCount m - 1]