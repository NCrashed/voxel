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
import Data.Voxel.Window
import Graphics.GPipe
import Reflex 

import qualified Data.MagicaVoxel as MV 
import qualified Data.Vector as GV
import qualified Graphics.GPipe.Context.GLFW as GLFW

runViewer :: IO () 
runViewer = runAppHost $ do 
    let initWidth = 800
    let initHeight = 600
    win <- createWindow "voxel viewer" initWidth initHeight
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

    ctx <- newPhongContext win camera
    runApp win $ viewerApp ctx scene

viewerApp :: forall t m os . MonadApp t os m 
  => PhongContext os
  -> Vector (SceneModel os)
  -> m ()
viewerApp ctx scene = do 
  frameB <- frameCounter
  let angBeh = do 
        frame <- frameB
        pure $ (0.005 * fromIntegral frame) `mod''` (2*pi)
  
  let capLod = min (GV.length scene - 1)
  let lodKeys = [GLFW.Key'0, GLFW.Key'1, GLFW.Key'2, GLFW.Key'3, GLFW.Key'4, GLFW.Key'5, GLFW.Key'6]
  keysEvs <- traverse keyPressed lodKeys
  let lodEv :: Event t Int = leftmost $ fmap (\(i, e) -> capLod i <$ e) $ [0 ..] `zip` keysEvs 
  lodBeh :: Behavior t Int <- fmap current $ holdDyn 0 lodEv

  setRenderer $ pure $ do
    ang <- sample angBeh
    lod <- sample lodBeh
    let modelTrans = rotateTransform (axisAngle (V3 0 0 1) ang) mempty
    renderModel ctx (scene GV.! lod) modelTrans

