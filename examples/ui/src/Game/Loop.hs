module Game.Loop(
    runGame
) where

import Data.Voxel.App
import Data.Voxel.App.Class
import Data.Voxel.Shader.Flat
import Data.Voxel.Texture 
import Data.Voxel.Window
import Graphics.GPipe

import qualified Data.Vector as V 

-- | Entry point to the game
runGame :: IO () 
runGame = runAppHost $ do 
  -- Allocate window
  let initWidth = 800
  let initHeight = 600
  win <- createWindow "UI example" initWidth initHeight
  -- Create player with LOD level but use only the most detailed one
  tex <- either error pure =<< loadTexture2D "./ui_atlas.png"
  let rects = V.fromList [Rect {
        rectPos = Region (V2 0.1 (1 - 0.1)) (V2 0.3 (1 - 0.3))
      , rectUv = Region 0 1
      , rectLevel = 0  
      }]
  buffs <- rectBuffers rects 

  -- Setup renderer
  ctx <- newFlatContext win tex
  -- Initiate rendering loop
  runApp win $ viewerApp ctx (rectBufferArray buffs) 

-- | Setup the inner game loop. The first phase when we 
-- allocates FRP network with events for player input.
--
-- The second part is rendering loop that samples the FRP 
-- network to render and update state of the game.
viewerApp :: forall t m os . MonadApp t os m 
  => FlatContext os
  -> LoadedRects os
  -> m ()
viewerApp ctx rect = do 
  -- Setup FRP network


  -- Rendering loop
  setRenderer $ pure $ do
    pure ()
    renderModel ctx rect
