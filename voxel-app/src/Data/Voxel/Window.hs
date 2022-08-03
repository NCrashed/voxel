module Data.Voxel.Window(
  createWindow
) where 

import Control.Monad.IO.Class
import Graphics.GPipe

import qualified Graphics.GPipe.Context.GLFW as GLFW

-- | Helper for making most common window
createWindow :: MonadIO m 
  => String -- ^ Title
  -> Int -- ^ Width 
  -> Int -- ^ Height
  -> ContextT GLFW.Handle os m (Window os RGBAFloat Depth) 
createWindow title initWidth initHeight = do 
  let wcfg = (GLFW.defaultWindowConfig title) {
          GLFW.configWidth = initWidth
        , GLFW.configHeight = initHeight
        }
  newWindow (WindowFormatColorDepth RGBA8 Depth16) wcfg

