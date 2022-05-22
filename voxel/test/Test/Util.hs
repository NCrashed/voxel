module Test.Util where

import Control.Exception
import Control.Monad
import Test.HUnit
import Test.QuickCheck.Arbitrary
import Linear

assertThrows :: IO a -> IO ()
assertThrows action =
    handleJust isWanted (const $ return ()) $ do
        action
        assertFailure $ "Expected exception but not thrown!"
  where
    isWanted :: SomeException -> Maybe ()
    isWanted _ = pure ()

instance Arbitrary a => Arbitrary (V3 a) where 
  arbitrary = V3 <$> arbitrary <*> arbitrary <*> arbitrary