module Data.Voxel.App.Class(
    SpiderCtx
  , Renderer
  , KeyEvent(..)
  , MouseEvent(..)
  , MonadGPipe(..)
  -- * Input helpers
  , keyPressed
  , keyDown
  -- * Window helpers
  , bindEscapeToClose
  -- * Reexports
  , Key(..), KeyState(..), ModifierKeys(..)
  , MouseButton(..), MouseButtonState(..)
  ) where 

import Control.Monad (void)
import Data.Word
import GHC.Generics (Generic)
import Graphics.GPipe
import Graphics.GPipe.Context.GLFW (Key(..), KeyState(..), ModifierKeys(..), MouseButton(..), MouseButtonState(..))
import Reflex

import qualified Graphics.GPipe.Context.GLFW as GLFW

-- | Shortcut for basic monad we work with. We have access to 
-- FRP timeline, OpenGL and GLFW context.
type SpiderCtx os = ContextT GLFW.Handle os (SpiderHost Global)

-- | Renderer function that is called every frame
type Renderer os = SpiderCtx os () 

-- | Fires for each press or repeat of keyboard keys (regardless of if it has textual meaning or not, eg Shift)
data KeyEvent = KeyEvent {
  _keyEvent_key      :: !Key 
, _keyEvent_scancode :: !Int 
, _keyEvent_state    :: !KeyState
, _keyEvent_mods     :: !ModifierKeys
} deriving (Show, Eq, Generic)

-- | Fires each time mouse button is pressed
data MouseEvent = MouseEvent {
  _mouseEvent_button :: !MouseButton
, _mouseEvent_state  :: !MouseButtonState
, _mouseEvent_mods   :: !ModifierKeys
} deriving (Show, Eq, Generic)

-- | API for GPipe renderer that are acceptable
-- inside FRP network.
class (Reflex t, Monad m) => MonadGPipe t os m | m -> t, m -> os where
  -- | Set shutdown event to indicate exit of the app.
  setShutdownEvent :: Event t () -> m ()
  -- | Get shutdown event
  shutdownEvent :: m (Event t ())
  -- | Get event that fires when window close is requested (X button clicked)
  windowCloseEvent :: m (Event t ())
  -- | Set current renderer
  setRenderer :: Behavior t (Renderer os) -> m ()
  -- | Get counter with frames rendered
  frameCounter :: m (Behavior t Word64)
  -- | Get event for key pressed
  keyInput :: m (Event t KeyEvent)
  -- | Get current mouse position
  mousePosition :: m (Behavior t (V2 Double))
  -- | Get event for mouse buttons
  mouseEvent :: m (Event t MouseEvent)
  -- | Get event for mouse scrolling
  mouseScroll :: m (Event t (V2 Double))

-- | Get event that fires when the key is pressed
keyPressed :: MonadGPipe t os m => Key -> m (Event t KeyEvent)
keyPressed k = do 
  e <- keyInput 
  let expected KeyEvent{..} = _keyEvent_key == k
  pure $ ffilter expected e

-- | Get dynamic that indicates whether the key is pressed right now
keyDown :: (MonadHold t m, MonadGPipe t os m) => Key -> m (Dynamic t Bool)
keyDown k = do
  e <- keyInput
  let expected KeyEvent{..} = _keyEvent_key == k
  let downE = ffor (ffilter expected e) $ \KeyEvent{..} ->
        _keyEvent_state == KeyState'Pressed || _keyEvent_state == KeyState'Repeating
  holdDyn False downE

-- | Wire Escape key press to trigger application shutdown.
-- Call this in your app to enable closing the window with Escape.
bindEscapeToClose :: (MonadHold t m, MonadGPipe t os m) => m ()
bindEscapeToClose = do
  escE <- keyPressed Key'Escape
  setShutdownEvent $ void escE