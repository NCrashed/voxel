{-# LANGUAGE RecursiveDo #-}
module Data.Voxel.UI.Base(
  UiEnv(..)
, newUiEnv
, UiT(..)
, runUiT
) where

import Control.Monad (when)
import Control.Monad.Exception
import Control.Monad.Fix (MonadFix)
import Control.Monad.Primitive
import Control.Monad.Reader 
import Control.Monad.Ref (MonadAtomicRef(..), MonadRef(..), Ref, readRef)
import Data.Coerce
import Data.IntMap (IntMap)
import Data.IORef
import Data.Vector (Vector)
import Data.Vector.Mutable (IOVector)
import Data.Voxel.Demand
import Data.Voxel.Rect 
import Data.Voxel.UI.Class
import GHC.Generics
import Reflex
import Reflex.Host.Class
import Reflex.Spider.Internal (HasSpiderTimeline)

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV 
import qualified Data.IntMap.Strict as M 

-- | ID for sub FRP network. That ID distinguish 
-- which part of UI should be rebuilt on switch 
-- events. 
type Generation = Int 

-- | ID of widget rect within one generation.  
type RectId = Int 

-- | Information of single generation of rects.
-- Each generation refers to FRP switch context, 
-- when the context is switched out, all generation 
-- is wiped out.
data GenerationInfo = GenerationInfo {
  genRectsRef :: !(IORef (IOVector Rect))
, genNextRect :: !RectId 
}

-- | Get only filled rects from generation 
getGenRects :: GenerationInfo -> IO (Vector Rect)
getGenRects GenerationInfo{..} = do 
  genRects <- readIORef genRectsRef
  v <- V.freeze genRects 
  pure $ V.take genNextRect v 

-- | Preallocate new generation
newGeneration :: IO GenerationInfo
newGeneration = do 
  let preallocRects = 100
  globalRects <- liftIO $ MV.new preallocRects
  globalRectsRef <- liftIO $ newIORef globalRects
  pure $ GenerationInfo globalRectsRef 0

-- | The internal environment of 'UiT'
data UiEnv t os = UiEnv {
    -- | We store all widgets rects to draw them lately.
    -- IntMap defines mapping for generations of FRP networks
    -- to actual vectors of mutable rects.
    _uiEnv_rects :: !(IORef (IntMap GenerationInfo))
    -- | Stores next free generation number to use for 
    -- dynamic FRP networks. 
  , _uiEnv_nextGen :: !(IORef Generation)
    -- | Current generation of the environment
  , _uiEnv_curGen :: !Generation
  } deriving (Generic)

newUiEnv :: (Reflex t, MonadIO m, TriggerEvent t m) => m (UiEnv t os)
newUiEnv = liftIO $ do
  newGen <- newGeneration
  rectsRef <- newIORef $ M.fromList [(0, newGen)]
  genRef <- newIORef 1
  pure UiEnv {
      _uiEnv_rects = rectsRef
    , _uiEnv_nextGen = genRef
    , _uiEnv_curGen = 0
    }

-- | Basic implementation of 'MonadUi'
newtype UiT t os m a = UiT { unUiT :: ReaderT (UiEnv t os) m a }
  deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadException, MonadAsyncException, Generic)

-- | Execute 'UiT' action with given environment.
runUiT :: Monad m => UiT t os m a -> UiEnv t os -> m a 
runUiT (UiT m) env = runReaderT m env  

instance (PerformEvent t m, MonadSample t m, MonadIO (Performable m), MonadIO m, MonadIO (PullM t), HasSpiderTimeline x, t ~ SpiderTimeline x) => MonadUI t (UiT t os m) where 
  {-# INLINABLE drawableRects #-}
  drawableRects = do
     ref <- UiT $ asks _uiEnv_rects
     pure $ onDemand $ do 
      m <- readIORef ref
      vs <- traverse getGenRects $ M.elems m 
      pure $ V.concat vs

  {-# INLINABLE drawRect #-}
  drawRect rectD = do
    UiEnv{..} <- UiT ask
    gens <- liftIO $ readIORef _uiEnv_rects
    GenerationInfo{..} <- maybe (error $ "drawRect: missing generation " ++ show _uiEnv_curGen) pure $ 
        M.lookup _uiEnv_curGen gens 

    -- Grow if needed
    liftIO $ do 
      genRects <- readIORef genRectsRef
      when (genNextRect >= MV.length genRects) $ do 
        let n = ceiling $ fromIntegral (MV.length genRects) * (1.5 :: Double)
        writeIORef genRectsRef =<< MV.grow genRects n
    
    -- Write initial value
    rect0 <- sample . current $ rectD
    liftIO $ do 
      genRects <- readIORef genRectsRef
      MV.write genRects genNextRect rect0
      -- Update generation
      let nextGens = GenerationInfo genRectsRef (genNextRect + 1)
      writeIORef _uiEnv_rects $! M.insert _uiEnv_curGen nextGens gens
    
    -- Update when the widget updated
    performEvent_ $ ffor (updated rectD) $ \rect -> liftIO $ do 
      genRects <- readIORef genRectsRef
      MV.write genRects genNextRect rect

instance MonadTrans (UiT t os) where
  {-# INLINABLE lift #-}
  lift = UiT . lift

instance PrimMonad m => PrimMonad (UiT t os m) where
  type PrimState (UiT t os m) = PrimState m
  {-# INLINABLE primitive #-}
  primitive = lift . primitive

instance PerformEvent t m => PerformEvent t (UiT t os m) where
  type Performable (UiT t os m) = Performable m
  {-# INLINABLE performEvent_ #-}
  performEvent_ e = lift $ performEvent_ e
  {-# INLINABLE performEvent #-}
  performEvent e = lift $ performEvent e

instance PostBuild t m => PostBuild t (UiT t os m) where
  {-# INLINABLE getPostBuild #-}
  getPostBuild = lift getPostBuild

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (UiT t os m) where
  {-# INLINABLE newEventWithTrigger #-}
  newEventWithTrigger = lift . newEventWithTrigger
  {-# INLINABLE newFanEventWithTrigger #-}
  newFanEventWithTrigger f = lift $ newFanEventWithTrigger f

instance TriggerEvent t m => TriggerEvent t (UiT t os m) where
  {-# INLINABLE newTriggerEvent #-}
  newTriggerEvent = lift newTriggerEvent
  {-# INLINABLE newTriggerEventWithOnComplete #-}
  newTriggerEventWithOnComplete = lift newTriggerEventWithOnComplete
  newEventWithLazyTriggerWithOnComplete = lift . newEventWithLazyTriggerWithOnComplete
  
instance MonadRef m => MonadRef (UiT t os m) where
  type Ref (UiT t os m) = Ref m
  {-# INLINABLE newRef #-}
  newRef = lift . newRef
  {-# INLINABLE readRef #-}
  readRef = lift . readRef
  {-# INLINABLE writeRef #-}
  writeRef r = lift . writeRef r

instance MonadAtomicRef m => MonadAtomicRef (UiT t os m) where
  {-# INLINABLE atomicModifyRef #-}
  atomicModifyRef r = lift . atomicModifyRef r

instance MonadSample t m => MonadSample t (UiT t os m) where
  {-# INLINABLE sample #-}
  sample = lift . sample

instance MonadHold t m => MonadHold t (UiT t os m) where
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

instance (MonadIO m, Adjustable t m) => Adjustable t (UiT t os m) where
  {-# INLINABLE runWithReplace #-}
  runWithReplace ma0 ea = do
    let switchNextGen env = liftIO $ do 
          i <- readIORef $ _uiEnv_nextGen env 
          writeIORef (_uiEnv_nextGen env) $! i+1
          newGen <- newGeneration
          modifyIORef (_uiEnv_rects env) $ M.insert i newGen
          pure i
        wipeCurGen env = liftIO $  
          modifyIORef (_uiEnv_rects env) $ M.delete (_uiEnv_curGen env)
    let ma0' = do 
          env <- UiT ask
          i <- switchNextGen env
          let env' = env {
                  _uiEnv_curGen = i 
                }
          UiT $ withReaderT (const env') $ unUiT ma0 
        ea' = ffor ea $ \m -> do 
          env <- UiT ask
          wipeCurGen env
          i <- switchNextGen env
          let env' = env {
                  _uiEnv_curGen = i 
                }
          UiT $ withReaderT (const env') $ unUiT m 
    UiT $ runWithReplace (unUiT ma0') (coerceEvent ea')
  {-# INLINABLE traverseIntMapWithKeyWithAdjust #-}
  traverseIntMapWithKeyWithAdjust f dm0 dm' = UiT $ traverseIntMapWithKeyWithAdjust (coerce . f) dm0 dm'
  {-# INLINABLE traverseDMapWithKeyWithAdjust #-}
  traverseDMapWithKeyWithAdjust f dm0 dm' = UiT $ traverseDMapWithKeyWithAdjust (coerce . f) dm0 dm'
  {-# INLINABLE traverseDMapWithKeyWithAdjustWithMove #-}
  traverseDMapWithKeyWithAdjustWithMove f dm0 dm' = UiT $ traverseDMapWithKeyWithAdjustWithMove (coerce . f) dm0 dm'

instance MonadReflexHost t m => MonadReflexHost t (UiT t os m) where 
  type ReadPhase (UiT t os m) = ReadPhase m
  fireEventsAndRead dm a = lift $ fireEventsAndRead dm a
  {-# INLINE fireEventsAndRead #-}
  runHostFrame = lift . runHostFrame
  {-# INLINE runHostFrame #-}

instance MonadSubscribeEvent t m => MonadSubscribeEvent t (UiT t os m) where
  subscribeEvent = lift . subscribeEvent

instance (ReflexHost t, NotReady t (HostFrame t)) => NotReady t (PerformEventT t (UiT t os m)) where
  notReadyUntil = PerformEventT . notReadyUntil
  notReady = PerformEventT notReady

instance NotReady t m => NotReady t (UiT t os m) where
  notReadyUntil = lift . notReadyUntil
  notReady = lift notReady
