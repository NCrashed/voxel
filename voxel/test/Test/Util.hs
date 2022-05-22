module Test.Util where

import Control.Exception
import Control.Monad
import Test.HUnit

assertThrows :: IO a -> IO ()
assertThrows action =
    handleJust isWanted (const $ return ()) $ do
        action
        assertFailure $ "Expected exception but not thrown!"
  where
    isWanted :: SomeException -> Maybe ()
    isWanted _ = pure ()
