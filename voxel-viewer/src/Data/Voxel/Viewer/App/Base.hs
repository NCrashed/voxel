{-# LANGUAGE RecursiveDo #-}
module Data.Voxel.Viewer.App.Base(
    GPipeEnv(..)
  , newGPipeEnv
  , bindEnvCallbacks
  , GPipeT(..)
  , runGPipeT
  ) where 

import Control.Monad.Exception
import Control.Monad.Primitive
import Control.Monad.Reader 
import Control.Monad.Ref (MonadAtomicRef(..), MonadRef(..), Ref, readRef)
import Data.Coerce
import Data.IORef
import Data.Voxel.Viewer.App.Class 
import GHC.Generics 
import Graphics.GPipe
import Reflex 
import Reflex.Host.Class

import qualified Graphics.GPipe.Context.GLFW as GLFW

-- | The internal environment of 'GPipeT'
data GPipeEnv t os = GPipeEnv { 
  -- | An event that requests application termination.
    _GPipeEnv_shutdown     :: Event t ()
  -- | Action that triggers previous event
  , _GPipeEnv_shutdownFire :: IO ()
  -- | Render function that is called each frame
  , _GPipeEnv_render       :: IORef (Behavior t (Renderer os))
  -- | An event that fires when a frame is rendered
  , _GPipeEnv_frame        :: Event t ()
  -- | Action that should be called after frame rendered
  , _GPipeEnv_frameFire    :: IO ()
  -- | Event that fired when key callback is called
  , _GPipeEnv_key          :: Event t KeyEvent
  -- | Fire command for callback for key press
  , _GPipeEnv_keyFire      :: KeyEvent -> IO ()
  }

newGPipeEnv :: (Reflex t, MonadIO m, TriggerEvent t m) => m (GPipeEnv t os)
newGPipeEnv = do
  (shutEv, shutdownFire) <- newTriggerEvent 
  (frameEv, frameFire) <- newTriggerEvent 
  (keyEv, keyFire) <- newTriggerEvent 
  rendererRef <- liftIO $ newIORef $ pure $ pure () 
  pure GPipeEnv {
      _GPipeEnv_shutdown = shutEv
    , _GPipeEnv_shutdownFire = shutdownFire () 
    , _GPipeEnv_render = rendererRef
    , _GPipeEnv_frame = frameEv
    , _GPipeEnv_frameFire = frameFire ()
    , _GPipeEnv_key = keyEv
    , _GPipeEnv_keyFire = keyFire
    }

-- | That should be called after 'newGPipeEnv' when FRP network is set.
bindEnvCallbacks :: MonadIO m => Window os c ds -> GPipeEnv t os -> ContextT GLFW.Handle os m ()
bindEnvCallbacks win GPipeEnv{..} = do 
  void $ GLFW.setKeyCallback win $ Just $ \key scan state mods -> _GPipeEnv_keyFire $! KeyEvent key scan state mods 

-- | Basic implementation of 'MonadGPipe'
newtype GPipeT t os m a = GPipeT { unGPipeT :: ReaderT (GPipeEnv t os) m a }
  deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadException, MonadAsyncException, Generic)

-- | Execute 'GPipeT' action with given environment.
runGPipeT :: Monad m => GPipeT t os m a -> GPipeEnv t os -> m a 
runGPipeT (GPipeT m) env = runReaderT m env  

instance (PerformEvent t m, MonadIO (Performable m), MonadIO m) => MonadGPipe t os (GPipeT t os m) where 
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

  {-# INLINE frameRendered #-}
  frameRendered = GPipeT $ asks _GPipeEnv_frame

  {-# INLINE keyInput #-}
  keyInput = GPipeT $ asks _GPipeEnv_key 
  
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
