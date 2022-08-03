module Data.Voxel.Scene(
    SceneModel
  , prepareVox
  , prepareGrid
  ) where

import Control.Monad.IO.Class
import Data.MagicaVoxel.Types (RGBA(..))
import Data.Voxel.Grid.Unbox.Polygon
import Data.Proxy 
import Data.Vector (Vector)
import Data.Voxel.GPipe.Mesh
import Data.Voxel.MagicaVoxel (convertMagica)
import Graphics.GPipe

import qualified Data.MagicaVoxel as MV 
import qualified Data.Voxel.Grid.Unbox as G
import qualified Data.Voxel.Mesh.Lod as L
import qualified Data.Vector as GV

-- | Loaded model to GPU to render 
type SceneModel os = Render
       os (PrimitiveArray Triangles (MeshArray (ArrayOf (V3 Float))))

prepareVox :: (MonadIO m, ContextHandler win) => MV.VoxFile -> ContextT win os m (Vector (SceneModel os))
prepareVox voxModel = do 
  rawModel <- either (fail . ("Vox convert: " ++)) pure $ convertMagica voxModel
  -- Create vertex data buffers
  let model :: G.VoxelGrid (V3 Float)
      model = G.map word32Color rawModel
  prepareGrid model 

prepareGrid :: (MonadIO m, ContextHandler win) => G.VoxelGrid (V3 Float) -> ContextT win os m (Vector (SceneModel os))
prepareGrid model = do 
  let renderModel :: L.LodMesh (V3 Float)
      renderModel = L.new TriangulateTriangles model
  buffers <- traverse meshBuffers $ L.allLods renderModel
  -- Make a Render action that returns a PrimitiveArray for the model
  let makeSingle = meshBufferArray (Proxy :: Proxy (V3 Float)) TriangleList
  let makePrimitives = GV.fromList $ makeSingle <$> buffers
  pure makePrimitives

word32Color :: RGBA -> V3 Float
word32Color (RGBA r g b _) = V3 r' g' b'
  where 
    r' = fromIntegral r / 255.0
    g' = fromIntegral g / 255.0
    b' = fromIntegral b / 255.0