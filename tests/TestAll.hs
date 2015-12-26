module Main where

import Test.Tasty
import qualified GameLogic as GL (testGroup) 

tests :: TestTree
tests = testGroup "All Tests" [GL.testGroup]

main :: IO ()
main = defaultMain tests
