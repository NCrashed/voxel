-- | Operations with polygonal meshes.
module Data.Voxel.Mesh(
    Mesh(..)
  , vertexNumber
  , triangleNumber
  , create
  , cube
  -- * Reexports
  , Storable
  ) where

import Data.Voxel.Mesh.Internal
import Foreign.Storable
