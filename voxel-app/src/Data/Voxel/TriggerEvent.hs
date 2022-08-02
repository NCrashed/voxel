-- | This module defines 'TriggerEventConcT', the implementation of
-- 'TriggerEvent' that uses TChan instead of Chan.
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Voxel.TriggerEvent
  ( TriggerEventConcT (..)
  , runTriggerEventConcT
  , askEventsConc
  , TriggerInvocation (..)
  , EventTriggerRef (..)
  ) where

import Control.Concurrent.STM.TChan 
import Control.Applicative (liftA2)
import Control.Monad.Exception
import Control.Monad.Primitive
import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.STM (atomically)
import Data.Coerce
import Data.Dependent.Sum
import qualified Data.Semigroup as S
import Reflex.Class
import Reflex.Adjustable.Class
import Reflex.Host.Class
import Reflex.PerformEvent.Class
import Reflex.PostBuild.Class
import Reflex.TriggerEvent.Class
import Reflex.TriggerEvent.Base (EventTriggerRef(..), TriggerInvocation(..))

-- | A basic implementation of 'TriggerEvent'.
newtype TriggerEventConcT t m a = TriggerEventConcT { unTriggerEventConcT :: ReaderT (TChan [DSum (EventTriggerRef t) TriggerInvocation]) m a }
  deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadException, MonadAsyncException)

-- | Run a 'TriggerEventConcT' action.  The argument should be a 'TChan' into which
-- 'TriggerInvocation's can be passed; it is expected that some other thread
-- will be responsible for popping values out of the 'TChan' and firing their
-- 'EventTrigger's.
runTriggerEventConcT :: TriggerEventConcT t m a -> TChan [DSum (EventTriggerRef t) TriggerInvocation] -> m a
runTriggerEventConcT = runReaderT . unTriggerEventConcT

instance MonadTrans (TriggerEventConcT t) where
  {-# INLINABLE lift #-}
  lift = TriggerEventConcT . lift

instance PrimMonad m => PrimMonad (TriggerEventConcT t m) where
  type PrimState (TriggerEventConcT t m) = PrimState m
  {-# INLINABLE primitive #-}
  primitive = lift . primitive

instance PerformEvent t m => PerformEvent t (TriggerEventConcT t m) where
  type Performable (TriggerEventConcT t m) = Performable m
  {-# INLINABLE performEvent_ #-}
  performEvent_ e = lift $ performEvent_ e
  {-# INLINABLE performEvent #-}
  performEvent e = lift $ performEvent e

instance PostBuild t m => PostBuild t (TriggerEventConcT t m) where
  {-# INLINABLE getPostBuild #-}
  getPostBuild = lift getPostBuild

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (TriggerEventConcT t m) where
  {-# INLINABLE newEventWithTrigger #-}
  newEventWithTrigger = lift . newEventWithTrigger
  {-# INLINABLE newFanEventWithTrigger #-}
  newFanEventWithTrigger f = lift $ newFanEventWithTrigger f

instance (Monad m, MonadRef m, Ref m ~ Ref IO, MonadReflexCreateTrigger t m) => TriggerEvent t (TriggerEventConcT t m) where
  {-# INLINABLE newTriggerEvent #-}
  newTriggerEvent = do
    (e, t) <- newTriggerEventWithOnComplete
    return (e, \a -> t a $ return ())
  {-# INLINABLE newTriggerEventWithOnComplete #-}
  newTriggerEventWithOnComplete = do
    events <- askEventsConc
    (eResult, reResultTrigger) <- lift newEventWithTriggerRef
    return . (,) eResult $ \a cb ->
      atomically $ writeTChan events [EventTriggerRef reResultTrigger :=> TriggerInvocation a cb]
  {-# INLINABLE newEventWithLazyTriggerWithOnComplete #-}
  newEventWithLazyTriggerWithOnComplete f = do
    events <- askEventsConc
    lift . newEventWithTrigger $ \t ->
      f $ \a cb -> do
        reResultTrigger <- newRef $ Just t
        atomically $ writeTChan events [EventTriggerRef reResultTrigger :=> TriggerInvocation a cb]

instance MonadRef m => MonadRef (TriggerEventConcT t m) where
  type Ref (TriggerEventConcT t m) = Ref m
  {-# INLINABLE newRef #-}
  newRef = lift . newRef
  {-# INLINABLE readRef #-}
  readRef = lift . readRef
  {-# INLINABLE writeRef #-}
  writeRef r = lift . writeRef r

instance MonadAtomicRef m => MonadAtomicRef (TriggerEventConcT t m) where
  {-# INLINABLE atomicModifyRef #-}
  atomicModifyRef r = lift . atomicModifyRef r

instance MonadSample t m => MonadSample t (TriggerEventConcT t m) where
  {-# INLINABLE sample #-}
  sample = lift . sample

instance MonadHold t m => MonadHold t (TriggerEventConcT t m) where
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

instance Adjustable t m => Adjustable t (TriggerEventConcT t m) where
  {-# INLINABLE runWithReplace #-}
  runWithReplace (TriggerEventConcT a0) a' = TriggerEventConcT $ runWithReplace a0 (coerceEvent a')
  {-# INLINABLE traverseIntMapWithKeyWithAdjust #-}
  traverseIntMapWithKeyWithAdjust f dm0 dm' = TriggerEventConcT $ traverseIntMapWithKeyWithAdjust (coerce . f) dm0 dm'
  {-# INLINABLE traverseDMapWithKeyWithAdjust #-}
  traverseDMapWithKeyWithAdjust f dm0 dm' = TriggerEventConcT $ traverseDMapWithKeyWithAdjust (coerce . f) dm0 dm'
  {-# INLINABLE traverseDMapWithKeyWithAdjustWithMove #-}
  traverseDMapWithKeyWithAdjustWithMove f dm0 dm' = TriggerEventConcT $ traverseDMapWithKeyWithAdjustWithMove (coerce . f) dm0 dm'

-- TODO: Monoid and Semigroup can likely be derived once ReaderT has them.
instance (Monoid a, Applicative m) => Monoid (TriggerEventConcT t m a) where
  mempty = pure mempty
  mappend = (<>)

instance (S.Semigroup a, Applicative m) => S.Semigroup (TriggerEventConcT t m a) where
  (<>) = liftA2 (S.<>)


-- | Retrieve the current 'TChan'; event trigger invocations pushed into it will
-- be fired.
askEventsConc :: Monad m => TriggerEventConcT t m (TChan [DSum (EventTriggerRef t) TriggerInvocation])
askEventsConc = TriggerEventConcT ask
