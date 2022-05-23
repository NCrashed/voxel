module Data.Voxel.MagicaVoxel(
    convertMagica
  , convertVoxModel
  ) where 

import Control.Monad.IO.Class
import Data.MagicaVoxel.Types (RGBA)
import Data.Maybe 
import Data.Voxel.Grid.Unbox (VoxelGrid)
import Data.Word 
import Linear

import qualified Data.MagicaVoxel as MV 
import qualified Data.Vector as V 
import qualified Data.Vector.Unboxed as VU 
import qualified Data.Voxel.Grid.Unbox as G 
import qualified Data.Voxel.Grid.Unbox.Mutable as GM 

convertMagica :: MV.VoxFile -> Either String (VoxelGrid RGBA)
convertMagica MV.VoxFile{..} 
  | V.null voxFileModels = Left "No models in the vox file!"
  | otherwise = Right $! convertVoxModel (V.head voxFileModels) (fromMaybe MV.defaultPalette voxFilePallete)

convertVoxModel :: MV.VoxModel -> MV.VoxPalette -> VoxelGrid RGBA
convertVoxModel MV.VoxModel{..} pallete = G.create $ do 
  g <- GM.new (V3 (fromIntegral voxModelX) (fromIntegral voxModelY) (fromIntegral voxModelZ))
  flip VU.mapM_ voxModelVoxels $ \(MV.Voxel x y z i) -> 
    GM.write g (V3 (fromIntegral x) (fromIntegral y) (fromIntegral z)) (pallete VU.! (fromIntegral i - 1))
  pure g
