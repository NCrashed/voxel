module Data.Voxel.MagicaVoxel(
    convertMagica
  ) where 

import Control.Monad.IO.Class 
import Data.Voxel.Mesh (Mesh)

import qualified Data.MagicaVoxel as MV 
import qualified Data.Vector as V 

convertMagica :: MV.VoxFile -> Either String (Mesh a)
convertMagica MV.VoxFile{..} 
  | V.null voxFileModels = Left "No models in the vox file!"
  | otherwise = convertVoxModel (V.head voxFileModels) 

convertVoxModel :: MV.VoxModel -> Either String (Mesh a)
convertVoxModel MV.VoxModel{..} = undefined
