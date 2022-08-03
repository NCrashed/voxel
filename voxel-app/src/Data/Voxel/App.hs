-- I shamelessly took some parts from reflex-vty by Obsidian Systems LLC 
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Voxel.App(
    MonadApp
  , App
  , SpiderCtx 
  , runAppHost 
  , runApp
  ) where 

import Control.Concurrent.STM.TChan (newTChanIO, tryReadTChan)
import Control.Monad (forM, forM_)
import Control.Monad.Exception (MonadException(..))
import Control.Monad.Fix (MonadFix, fix)
import Control.Monad.Identity (Identity(..))
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Ref (MonadRef(..), Ref, readRef)
import Control.Monad.STM (atomically)
import Control.Monad.Trans.Class (lift)
import Data.Dependent.Sum (DSum ((:=>)))
import Data.IORef (IORef, readIORef, modifyIORef')
import Data.Maybe (catMaybes, fromMaybe)
import Data.Voxel.App.Class
import Data.Voxel.App.Base
import Data.Voxel.TriggerEvent
import Graphics.GPipe
import Reflex
import Reflex.Host.Class

import qualified Graphics.GPipe.Context.GLFW as GLFW

-- | Basic monad for FRP application. See 'runViewer' implementation
-- to see how these constaints are fullfilled.
type MonadApp t os m = (
    Adjustable t m
  , MonadException m
  , MonadFix m
  , MonadHold t m
  , MonadIO (HostFrame t)
  , MonadIO (Performable m)
  , MonadIO m
  , MonadRef (HostFrame t)
  , NotReady t (HostFrame t)
  , NotReady t m
  , PerformEvent t m
  , PostBuild t m
  , PrimMonad (HostFrame t)
  , Ref (HostFrame t) ~ IORef
  , Ref m ~ IORef
  , Reflex t
  , ReflexHost t
  , TriggerEvent t m
  , MonadGPipe t os m
  , MonadIO (PullM t)
  , t ~ SpiderTimeline Global
  )

-- | Part of application that can use FRP API
type App t os m = MonadApp t os m => m ()

-- | Helper to run FRP host frame to run appplication inside it
runAppHost :: (forall os . SpiderCtx os a) -> IO a
runAppHost m =  
  -- We are using the 'Spider' implementation of reflex. Running the host
  -- allows us to take actions on the FRP timeline. The scoped type signature
  -- specifies that our host runs on the Global timeline.
  -- For more information, see 'Reflex.Spider.Internal.runSpiderHost'.
  (runSpiderHost :: SpiderHost Global a -> IO a) $ runContextT GLFW.defaultHandleConfig m 

-- | Runs infinite loop of rendering and event handling of the App.
runApp :: 
  -- | Main window for the application
     Window os RGBAFloat Depth
  -- | Application that provided by user of the function
  -> (forall t m . App t os m) 
  -> SpiderCtx os ()
runApp win app = do
    -- Create the "post-build" event and associated trigger. This event fires
    -- once, when the application starts.
    (postBuild, postBuildTriggerRef) <- lift newEventWithTriggerRef
    
    -- Create a queue to which we will write 'Event's that need to be
    -- processed.
    events <- liftIO newTChanIO

    -- Run the "guest" application, providing the appropriate context. The
    -- result is a 'GPipeEnv', and a 'FireCommand' that will be used to
    -- trigger events.
    (env, fc@(FireCommand fire)) <- do
      hostPerformEventT $                 -- Allows the guest app to run
                                          -- 'performEvent', so that actions
                                          -- (e.g., IO actions) can be run when
                                          -- 'Event's fire.

        flip runPostBuildT postBuild $    -- Allows the guest app to access to
                                          -- a "post-build" 'Event'

          flip runTriggerEventConcT events $ do  -- Allows the guest app to create new
                                                 -- events and triggers and writes
                                                 -- those triggers to a channel from
                                                 -- which they will be read and
                                                 -- processed.
            env <- newGPipeEnv
            runGPipeT app env
            pure env
                                          -- The guest app is provided the
                                          -- initial environment, executed with it.
                                          -- App setups all required actions and then we 
                                          -- retrieve everything from the returned env.
    -- Bind all required callbacks to the FRP network 
    -- that we allocated on the previous step.
    bindEnvCallbacks win env 

    -- Read the trigger reference for the post-build event. This will be
    -- 'Nothing' if the guest application hasn't subscribed to this event.
    mPostBuildTrigger <- readRef postBuildTriggerRef

    -- When there is a subscriber to the post-build event, fire the event.
    forM_ mPostBuildTrigger $ \postBuildTrigger ->
      fire [postBuildTrigger :=> Identity ()] $ return ()

    -- Subscribe to an 'Event' of that the guest application can use to
    -- request application shutdown. We'll check whether this 'Event' is firing
    -- to determine whether to terminate.
    shutdown <- subscribeEvent $ _GPipeEnv_shutdown env
    
    -- The main application loop. We wait for new events, fire those that
    -- have subscribers, and update the display. If we detect a shutdown
    -- request, the application terminates.
    fix $ \loop -> do
      -- Read the next event (non-blocking).
      ers <- liftIO . atomically $ tryReadTChan events
      stop <- do
        -- Fire events that have subscribers.
        fireEventTriggerRefs fc (fromMaybe [] ers) $
          -- Check if the shutdown 'Event' is firing.
          readEvent shutdown >>= \case
            Nothing -> return False
            Just _ -> return True
      if or stop then pure ()                
        else do  -- Otherwise, update the display and loop.
          renderBehavior <- liftIO $ readIORef $ _GPipeEnv_render env
          renderFn <- sample renderBehavior
          renderFn
          liftIO $ modifyIORef' (_GPipeEnv_frame env) (+1)
          loop

  where
    -- | Use the given 'FireCommand' to fire events that have subscribers
    -- and call the callback for the 'TriggerInvocation' of each.
    fireEventTriggerRefs
      :: forall t m a . (Monad (ReadPhase m), MonadIO m)
      => FireCommand t m
      -> [DSum (EventTriggerRef t) TriggerInvocation]
      -> ReadPhase m a
      -> m [a]
    fireEventTriggerRefs (FireCommand fire) ers rcb = do
      mes <- liftIO $
        forM ers $ \(EventTriggerRef er :=> TriggerInvocation a _) -> do
          me <- readIORef er
          return $ fmap (\e -> e :=> Identity a) me
      a <- fire (catMaybes mes) rcb
      liftIO $ forM_ ers $ \(_ :=> TriggerInvocation _ cb) -> cb
      return a

instance MonadRef m => MonadRef (ContextT handle os m) where 
  type Ref (ContextT handle os m) = Ref m

  newRef     r   = lift $ newRef     r
  readRef    r   = lift $ readRef    r
  writeRef   r x = lift $ writeRef   r x
  modifyRef  r f = lift $ modifyRef  r f
  modifyRef' r f = lift $ modifyRef' r f

instance MonadReflexHost t m => MonadReflexHost t (ContextT handle os m) where 
  type ReadPhase (ContextT handle os m) = ReadPhase m
  fireEventsAndRead dm a = lift $ fireEventsAndRead dm a
  {-# INLINE fireEventsAndRead #-}
  runHostFrame = lift . runHostFrame
  {-# INLINE runHostFrame #-}

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (ContextT handle os m) where
  newEventWithTrigger = lift . newEventWithTrigger
  newFanEventWithTrigger initializer = lift $ newFanEventWithTrigger initializer

instance MonadSubscribeEvent t m => MonadSubscribeEvent t (ContextT handle os m) where
  subscribeEvent = lift . subscribeEvent

instance MonadSample t m => MonadSample t (ContextT handle os m) where
  sample = lift . sample

instance MonadHold t m => MonadHold t (ContextT handle os m) where
  hold a0 = lift . hold a0
  holdDyn a0 = lift . holdDyn a0
  holdIncremental a0 = lift . holdIncremental a0
  buildDynamic a0 = lift . buildDynamic a0
  headE = lift . headE
  now = lift now

instance (ReflexHost t, NotReady t (HostFrame t)) => NotReady t (PerformEventT t (ContextT handle os m)) where
  notReadyUntil = PerformEventT . notReadyUntil
  notReady = PerformEventT notReady

instance NotReady t m => NotReady t (TriggerEventConcT t m) where
  notReadyUntil = lift . notReadyUntil
  notReady = lift notReady
