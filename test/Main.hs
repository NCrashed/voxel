module Main where

import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [qcProperties, unitTests]

qcProperties :: TestTree
qcProperties = testGroup "Properties" [

  ]

unitTests :: TestTree
unitTests = testGroup "Unit tests" [

  ]
