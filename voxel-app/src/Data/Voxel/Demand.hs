module Data.Voxel.Demand (onDemand) where

import Control.Monad.ReaderIO (ReaderIO(..))
import Data.IORef (newIORef)
import qualified Reflex.Spider.Internal as Spider
import Reflex (Behavior)

onDemand :: IO a -> Behavior (Spider.SpiderTimeline x) a
onDemand ma = Spider.SpiderBehavior . Spider.Behavior . Spider.BehaviorM . ReaderIO $ computeF
  where
    {-# NOINLINE computeF #-}
    computeF (Nothing, _) = ma
    computeF (Just (invW,_), _) = do
        toReconnect <- newIORef []
        _ <- Spider.invalidate toReconnect [invW]
        ma