{-# LANGUAGE RecursiveDo #-}
module Data.Voxel.App.Base(
    GPipeEnv(..)
  , newGPipeEnv
  , bindEnvCallbacks
  , GPipeT(..)
  , runGPipeT
  ) where 

import Control.Monad.Exception
import Control.Monad (void)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Primitive
import Control.Monad.Reader 
import Control.Monad.Ref (MonadAtomicRef(..), MonadRef(..), Ref, readRef)
import Data.Coerce
import Data.IORef
import Data.Voxel.App.Class 
import Data.Voxel.Demand
import Data.Word
import GHC.Generics 
import Graphics.GPipe
import Reflex
import Reflex.Host.Class
import Reflex.Spider.Internal (HasSpiderTimeline)

import qualified Graphics.GPipe.Context.GLFW as GLFW

-- | The internal environment of 'GPipeT'
data GPipeEnv t os = GPipeEnv {
  -- | An event that requests application termination.
    _GPipeEnv_shutdown     :: Event t ()
  -- | Action that triggers previous event
  , _GPipeEnv_shutdownFire :: IO ()
  -- | Render function that is called each frame
  , _GPipeEnv_render       :: IORef (Behavior t (Renderer os))
  -- | Frame counter that updated after each draw
  , _GPipeEnv_frame        :: IORef Word64
  -- | Event that fires each frame with the frame number
  , _GPipeEnv_frameEvent   :: Event t Word64
  -- | Fire command for frame event
  , _GPipeEnv_frameFire    :: Word64 -> IO ()
  -- | Event that fired when key callback is called
  , _GPipeEnv_key          :: Event t KeyEvent
  -- | Fire command for callback for key press
  , _GPipeEnv_keyFire      :: KeyEvent -> IO ()
  -- | Current position of mouse cursor
  , _GPipeEnv_mousePos     :: IORef (V2 Double)
  -- | Event for mouse buttons
  , _GPipeEnv_mouseEvent   :: Event t MouseEvent
  -- | Fire command for callback that catches mouse button events
  , _GPipeEnv_mouseFire    :: MouseEvent -> IO ()
  -- | Event that fires when mouse scroll occurs
  , _GPipeEnv_scrollEvent  :: Event t (V2 Double)
  -- | Fire for the scroll event. Called in callback by GLFW.
  , _GPipeEnv_scrollFire   :: V2 Double -> IO ()
  }

newGPipeEnv :: (Reflex t, MonadIO m, TriggerEvent t m) => m (GPipeEnv t os)
newGPipeEnv = do
  (shutEv, shutdownFire) <- newTriggerEvent
  (keyEv, keyFire) <- newTriggerEvent
  (frameEv, frameFire) <- newTriggerEvent
  rendererRef <- liftIO $ newIORef $ pure $ pure ()
  frameRef <- liftIO $ newIORef 0
  mousePosRef <- liftIO $ newIORef 0
  (mouseButtonEv, mouseButtonFire) <- newTriggerEvent
  (scrollEv, scrollFire) <- newTriggerEvent
  pure GPipeEnv {
      _GPipeEnv_shutdown = shutEv
    , _GPipeEnv_shutdownFire = shutdownFire ()
    , _GPipeEnv_render = rendererRef
    , _GPipeEnv_frame = frameRef
    , _GPipeEnv_frameEvent = frameEv
    , _GPipeEnv_frameFire = frameFire
    , _GPipeEnv_key = keyEv
    , _GPipeEnv_keyFire = keyFire
    , _GPipeEnv_mousePos = mousePosRef
    , _GPipeEnv_mouseEvent = mouseButtonEv
    , _GPipeEnv_mouseFire = mouseButtonFire
    , _GPipeEnv_scrollEvent = scrollEv
    , _GPipeEnv_scrollFire = scrollFire
    }

-- | That should be called after 'newGPipeEnv' when FRP network is set.
bindEnvCallbacks :: MonadIO m => Window os c ds -> GPipeEnv t os -> ContextT GLFW.Handle os m ()
bindEnvCallbacks win GPipeEnv{..} = do 
  void $ GLFW.setKeyCallback win $ Just $ \key scan state mods -> 
    _GPipeEnv_keyFire $! KeyEvent key scan state mods 
  void $ GLFW.setCursorPosCallback win $ Just $ \x y -> 
    writeIORef _GPipeEnv_mousePos $! V2 x y 
  void $ GLFW.setMouseButtonCallback win $ Just $ \button state mods -> 
    _GPipeEnv_mouseFire $! MouseEvent button state mods
  void $ GLFW.setScrollCallback win $ Just $ \x y -> 
    _GPipeEnv_scrollFire $! V2 x y

-- | Basic implementation of 'MonadGPipe'
newtype GPipeT t os m a = GPipeT { unGPipeT :: ReaderT (GPipeEnv t os) m a }
  deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadException, MonadAsyncException, Generic)

-- | Execute 'GPipeT' action with given environment.
runGPipeT :: Monad m => GPipeT t os m a -> GPipeEnv t os -> m a 
runGPipeT (GPipeT m) env = runReaderT m env  

instance (PerformEvent t m, MonadHold t m, MonadIO (Performable m), MonadIO m, MonadIO (PullM t), HasSpiderTimeline x, t ~ SpiderTimeline x) => MonadGPipe t os (GPipeT t os m) where
  {-# INLINABLE setShutdownEvent #-}
  setShutdownEvent e = do
    env <- GPipeT ask
    performEvent_ $ liftIO (_GPipeEnv_shutdownFire env) <$ e

  {-# INLINE shutdownEvent #-}
  shutdownEvent = GPipeT $ asks _GPipeEnv_shutdown

  {-# INLINABLE setRenderer #-}
  setRenderer r = do
    env <- GPipeT ask
    liftIO $ writeIORef (_GPipeEnv_render env) r

  {-# INLINE frameCounter #-}
  frameCounter = do
    frameEv <- GPipeT $ asks _GPipeEnv_frameEvent
    fmap current $ holdDyn 0 frameEv

  {-# INLINE keyInput #-}
  keyInput = GPipeT $ asks _GPipeEnv_key

  {-# INLINE mousePosition #-}
  mousePosition = do
    ref <- GPipeT $ asks _GPipeEnv_mousePos
    pure $ onDemand $ readIORef ref

  {-# INLINE mouseEvent #-}
  mouseEvent = GPipeT $ asks _GPipeEnv_mouseEvent

  {-# INLINE mouseScroll #-}
  mouseScroll = GPipeT $ asks _GPipeEnv_scrollEvent

instance MonadTrans (GPipeT t os) where
  {-# INLINABLE lift #-}
  lift = GPipeT . lift

instance PrimMonad m => PrimMonad (GPipeT t os m) where
  type PrimState (GPipeT t os m) = PrimState m
  {-# INLINABLE primitive #-}
  primitive = lift . primitive

instance PerformEvent t m => PerformEvent t (GPipeT t os m) where
  type Performable (GPipeT t os m) = Performable m
  {-# INLINABLE performEvent_ #-}
  performEvent_ e = lift $ performEvent_ e
  {-# INLINABLE performEvent #-}
  performEvent e = lift $ performEvent e

instance PostBuild t m => PostBuild t (GPipeT t os m) where
  {-# INLINABLE getPostBuild #-}
  getPostBuild = lift getPostBuild

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (GPipeT t os m) where
  {-# INLINABLE newEventWithTrigger #-}
  newEventWithTrigger = lift . newEventWithTrigger
  {-# INLINABLE newFanEventWithTrigger #-}
  newFanEventWithTrigger f = lift $ newFanEventWithTrigger f

instance TriggerEvent t m => TriggerEvent t (GPipeT t os m) where
  {-# INLINABLE newTriggerEvent #-}
  newTriggerEvent = lift newTriggerEvent
  {-# INLINABLE newTriggerEventWithOnComplete #-}
  newTriggerEventWithOnComplete = lift newTriggerEventWithOnComplete
  newEventWithLazyTriggerWithOnComplete = lift . newEventWithLazyTriggerWithOnComplete
  
instance MonadRef m => MonadRef (GPipeT t os m) where
  type Ref (GPipeT t os m) = Ref m
  {-# INLINABLE newRef #-}
  newRef = lift . newRef
  {-# INLINABLE readRef #-}
  readRef = lift . readRef
  {-# INLINABLE writeRef #-}
  writeRef r = lift . writeRef r

instance MonadAtomicRef m => MonadAtomicRef (GPipeT t os m) where
  {-# INLINABLE atomicModifyRef #-}
  atomicModifyRef r = lift . atomicModifyRef r

instance MonadSample t m => MonadSample t (GPipeT t os m) where
  {-# INLINABLE sample #-}
  sample = lift . sample

instance MonadHold t m => MonadHold t (GPipeT t os m) where
  {-# INLINABLE hold #-}
  hold v0 v' = lift $ hold v0 v'
  {-# INLINABLE holdDyn #-}
  holdDyn v0 v' = lift $ holdDyn v0 v'
  {-# INLINABLE holdIncremental #-}
  holdIncremental v0 v' = lift $ holdIncremental v0 v'
  {-# INLINABLE buildDynamic #-}
  buildDynamic a0 = lift . buildDynamic a0
  {-# INLINABLE headE #-}
  headE = lift . headE
  {-# INLINABLE now #-}
  now = lift now

instance Adjustable t m => Adjustable t (GPipeT t os m) where
  {-# INLINABLE runWithReplace #-}
  runWithReplace (GPipeT a0) a' = GPipeT $ runWithReplace a0 (coerceEvent a')
  {-# INLINABLE traverseIntMapWithKeyWithAdjust #-}
  traverseIntMapWithKeyWithAdjust f dm0 dm' = GPipeT $ traverseIntMapWithKeyWithAdjust (coerce . f) dm0 dm'
  {-# INLINABLE traverseDMapWithKeyWithAdjust #-}
  traverseDMapWithKeyWithAdjust f dm0 dm' = GPipeT $ traverseDMapWithKeyWithAdjust (coerce . f) dm0 dm'
  {-# INLINABLE traverseDMapWithKeyWithAdjustWithMove #-}
  traverseDMapWithKeyWithAdjustWithMove f dm0 dm' = GPipeT $ traverseDMapWithKeyWithAdjustWithMove (coerce . f) dm0 dm'

instance MonadReflexHost t m => MonadReflexHost t (GPipeT t os m) where 
  type ReadPhase (GPipeT t os m) = ReadPhase m
  fireEventsAndRead dm a = lift $ fireEventsAndRead dm a
  {-# INLINE fireEventsAndRead #-}
  runHostFrame = lift . runHostFrame
  {-# INLINE runHostFrame #-}

instance MonadSubscribeEvent t m => MonadSubscribeEvent t (GPipeT t os m) where
  subscribeEvent = lift . subscribeEvent

instance (ReflexHost t, NotReady t (HostFrame t)) => NotReady t (PerformEventT t (GPipeT t os m)) where
  notReadyUntil = PerformEventT . notReadyUntil
  notReady = PerformEventT notReady

instance NotReady t m => NotReady t (GPipeT t os m) where
  notReadyUntil = lift . notReadyUntil
  notReady = lift notReady
