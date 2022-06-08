module Data.Voxel.Viewer.App.Class(
    Renderer
  , KeyEvent(..)
  , MonadGPipe(..)
  -- * Input helpers
  , keyPressed
  -- * Reexports 
  , Key(..), KeyState(..), ModifierKeys(..)
  ) where 

import GHC.Generics (Generic)
import Graphics.GPipe
import Graphics.GPipe.Context.GLFW (Key(..), KeyState(..), ModifierKeys(..))
import Reflex

import qualified Graphics.GPipe.Context.GLFW as GLFW

-- | Renderer function that is called every frame
type Renderer os = ContextT GLFW.Handle os (SpiderHost Global) () 

-- | Fires for each press or repeat of keyboard keys (regardless of if it has textual meaning or not, eg Shift)
data KeyEvent = KeyEvent {
  _keyEvent_key      :: !Key 
, _keyEvent_scancode :: !Int 
, _keyEvent_state    :: !KeyState
, _keyEvent_mods     :: !ModifierKeys
} deriving (Show, Eq, Generic)

-- | API for GPipe renderer that are acceptable 
-- inside FRP network.
class (Reflex t, Monad m) => MonadGPipe t os m | m -> t, m -> os where
  -- | Set shutdown event to indicate exit of the app.
  setShutdownEvent :: Event t () -> m ()
  -- | Get shutdown event
  shutdownEvent :: m (Event t ())
  -- | Set current renderer
  setRenderer :: Behavior t (Renderer os) -> m ()
  -- | Get event when a single frame is rendered
  frameRendered :: m (Event t ())
  -- | Get event for key pressed
  keyInput :: m (Event t KeyEvent)

-- | Get event that fires when the key is pressed
keyPressed :: MonadGPipe t os m => Key -> m (Event t KeyEvent)
keyPressed k = do 
  e <- keyInput 
  let expected KeyEvent{..} = _keyEvent_key == k
  pure $ ffilter expected e

