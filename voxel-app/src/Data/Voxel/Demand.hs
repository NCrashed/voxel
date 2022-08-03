module Data.Voxel.Demand (onDemand) where 

import Control.Monad.ReaderIO (ReaderIO(..))
import Data.IORef (newIORef)
import qualified Reflex.Spider.Internal as Spider
import Reflex (Behavior)
import System.IO.Unsafe (unsafeInterleaveIO)

onDemand :: IO a -> Behavior (Spider.SpiderTimeline x) a
onDemand ma = Spider.SpiderBehavior . Spider.Behavior . Spider.BehaviorM . ReaderIO $ computeF
  where
    {-# NOINLINE computeF #-}
    computeF (Nothing, _) = unsafeInterleaveIO ma
    computeF (Just (invW,_), _) = unsafeInterleaveIO $ do
        toReconnect <- newIORef []
        _ <- Spider.invalidate toReconnect [invW]
        ma