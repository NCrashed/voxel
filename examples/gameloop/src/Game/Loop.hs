module Game.Loop(
    runGame
) where

import Control.Monad.IO.Class
import Data.Vector (Vector)
import Data.Voxel.App
import Data.Voxel.App.Class
import Data.Voxel.Camera
import Data.Voxel.Scene
import Data.Voxel.Shader.Phong
import Data.Voxel.Transform 
import Game.Player 
import Graphics.GPipe
import Reflex 

import qualified Graphics.GPipe.Context.GLFW as GLFW
import qualified Data.Vector as V 

runGame :: IO () 
runGame = 
  -- We are using the 'Spider' implementation of reflex. Running the host
  -- allows us to take actions on the FRP timeline. The scoped type signature
  -- specifies that our host runs on the Global timeline.
  -- For more information, see 'Reflex.Spider.Internal.runSpiderHost'.
  (runSpiderHost :: SpiderHost Global a -> IO a) $ runContextT GLFW.defaultHandleConfig $ do 
    let initWidth = 800
    let initHeight = 600
    win <- createWindow initWidth initHeight
    matBuffer <- newBuffer 1
    shader <- compileShader $ pipelineShader win matBuffer 
    scene <- prepareGrid playerModel

    let camera :: Camera Float
        camera = Camera {
          cameraPosition = - V3 0 0 5
        , cameraRotation = axisAngle (V3 0 0 1) 0
        , cameraAngle = (pi/9)
        , cameraAspect = (fromIntegral initWidth / fromIntegral initHeight)
        , cameraNear = 1 
        , cameraFar = 100
        }

    runApp win $ viewerApp win camera shader scene matBuffer

viewerApp :: forall t m os . MonadApp t os m 
  => Window os RGBAFloat Depth
  -> Camera Float
  -> CompiledShader os ShaderEnvironment
  -> Vector (SceneModel os)
  -> MatrixUniform os
  -> m ()
viewerApp win camera shader scene matBuffer = do 
  frameE <- frameRendered
  angBeh <- fmap current $ foldDyn (\_ ang -> (ang + 0.005) `mod''` (2*pi)) 0.0 frameE
  
  setRenderer $ pure $ do
    ang <- sample angBeh
    let ctx = RenderContext {
          renderWindow = win 
        , renderShader = shader 
        , renderMatrix = matBuffer 
        , renderCamera = rotateCamera (axisAngle (V3 0 0 1) ang) camera 
        }
    renderModel ctx (V.head scene) notransform

createWindow :: MonadIO m => Int -> Int -> ContextT GLFW.Handle os m (Window os RGBAFloat Depth) 
createWindow initWidth initHeight = do 
  let wcfg = (GLFW.defaultWindowConfig "Game loop example") {
          GLFW.configWidth = initWidth
        , GLFW.configHeight = initHeight
        }
  newWindow (WindowFormatColorDepth RGBA8 Depth16) wcfg


