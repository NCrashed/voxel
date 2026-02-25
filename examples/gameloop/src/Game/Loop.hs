module Game.Loop(
    runGame
) where

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Data.Voxel.App
import Data.Voxel.App.Class
import Data.Voxel.Camera
import Data.Voxel.Scene
import Data.Voxel.Shader.Phong
import Data.Voxel.Transform 
import Data.Voxel.Window
import Game.Player 
import Graphics.GPipe
import Reflex 

import qualified Graphics.GPipe.Context.GLFW as GLFW
import qualified Data.Vector as V 

-- | Entry point to the game
runGame :: IO () 
runGame = runAppHost $ do 
  -- Allocate window
  let initWidth = 800
  let initHeight = 600
  win <- createWindow "Game loop example" initWidth initHeight
  -- Create player with LOD level but use only the most detailed one
  scene <- prepareGrid playerModel

  -- Setup renderer
  let camera :: Camera Float
      camera = Camera {
        cameraPosition = - V3 0 0 25
      , cameraRotation = axisAngle (V3 0 0 1) 0
      , cameraAngle = (pi/9)
      , cameraAspect = (fromIntegral initWidth / fromIntegral initHeight)
      , cameraNear = 1 
      , cameraFar = 100
      }
  ctx <- newPhongContext win camera
  -- Initiate rendering loop
  runApp win $ viewerApp ctx (V.head scene)

-- | Setup the inner game loop. The first phase when we 
-- allocates FRP network with events for player input.
--
-- The second part is rendering loop that samples the FRP 
-- network to render and update state of the game.
viewerApp :: forall t m os . MonadApp t os m
  => PhongContext os
  -> SceneModel os
  -> m ()
viewerApp ctx player = do
  -- Setup FRP network
  bindEscapeToClose
  moveD <- playerMove 
  posRef <- liftIO $ newIORef mempty
  -- Some demo printing of input 
  mouseE <- mouseEvent
  performEvent_ $ (liftIO . print) <$> mouseE
  scrollE <- mouseScroll 
  performEvent_ $ (liftIO . print) <$> scrollE
  mousePosB <- mousePosition
  mousePosRef <- liftIO $ newIORef 0

  -- Rendering loop
  setRenderer $ pure $ do
    move <- sample . current $ moveD
    mousePos <- sample mousePosB
    pos <- liftIO $ do 
      -- Show position
      oldPos <- readIORef mousePosRef
      unless (oldPos == mousePos) $ do 
        print mousePos 
        writeIORef mousePosRef mousePos 
      -- Collect positions
      modifyIORef' posRef (translateTransform move)
      readIORef posRef 
    renderModel ctx player pos 

-- | Calculate next diff of the player position.
-- Note that we are not accumulating the translation,
-- it is done in the main loop.
playerMove :: forall t m os . MonadApp t os m 
  => m (Dynamic t (V3 Float)) 
playerMove = do 
  let attachVec val b = ffor b $ \v -> if v then val else 0   
  upB <- attachVec (V3 0 0.1 0) <$> keyDown GLFW.Key'Up
  downB <- attachVec (V3 0 (-0.1) 0) <$> keyDown GLFW.Key'Down
  leftB <- attachVec (V3 (-0.1) 0 0) <$> keyDown GLFW.Key'Left
  rightB <- attachVec (V3 0.1 0 0) <$> keyDown GLFW.Key'Right 
  pure $ sum <$> sequence [upB, downB, leftB, rightB]

