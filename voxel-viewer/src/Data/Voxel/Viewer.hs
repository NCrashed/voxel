module Data.Voxel.Viewer(
    runViewer
  , viewerApp
  ) where

import Control.Monad.IO.Class
import Data.Vector (Vector)
import Data.Voxel.App
import Data.Voxel.App.Class
import Data.Voxel.Camera
import Data.Voxel.Scene
import Data.Voxel.Shader.Phong
import Data.Voxel.Transform
import Graphics.GPipe
import Reflex 

import qualified Data.MagicaVoxel as MV 
import qualified Data.Vector as GV
import qualified Graphics.GPipe.Context.GLFW as GLFW

runViewer :: IO () 
runViewer = 
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
    voxModel <- either (fail . ("Vox loading: " ++)) pure =<< MV.parseFile "../MagicaVoxel-vox/test/harvester_full.vox" 
    scene <- prepareVox voxModel

    let camera :: Camera Float
        camera = Camera {
          cameraPosition = - V3 0 0 5
        , cameraRotation = axisAngle (V3 1 0 0) (-pi/3)
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
  
  let capLod = min (GV.length scene - 1)
  let lodKeys = [GLFW.Key'0, GLFW.Key'1, GLFW.Key'2, GLFW.Key'3, GLFW.Key'4, GLFW.Key'5, GLFW.Key'6]
  keysEvs <- traverse keyPressed lodKeys
  let lodEv :: Event t Int = leftmost $ fmap (\(i, e) -> capLod i <$ e) $ [0 ..] `zip` keysEvs 
  lodBeh :: Behavior t Int <- fmap current $ holdDyn 0 lodEv

  setRenderer $ pure $ do
    ang <- sample angBeh
    lod <- sample lodBeh
    let ctx = RenderContext {
          renderWindow = win 
        , renderShader = shader 
        , renderMatrix = matBuffer
        , renderCamera = camera
        }
    let modelTrans = rotateTransform (axisAngle (V3 0 0 1) ang) mempty
    renderModel ctx (scene GV.! lod) modelTrans

createWindow :: MonadIO m => Int -> Int -> ContextT GLFW.Handle os m (Window os RGBAFloat Depth) 
createWindow initWidth initHeight = do 
  let wcfg = (GLFW.defaultWindowConfig "voxel viewer") {
          GLFW.configWidth = initWidth
        , GLFW.configHeight = initHeight
        }
  newWindow (WindowFormatColorDepth RGBA8 Depth16) wcfg


