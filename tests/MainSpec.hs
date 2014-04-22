module Main where

 
import   NumericalUnit.Layout 
import   NumericalUnit.Shape 

import Test.Tasty
--import Test.Tasty.SmallCheck as SC
--import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.List
import Data.Ord

main = defaultMain tests

tests :: TestTree
tests = testGroup "Unit Tests" [unitTestShape] -- , unitTestLayout ]


--unitTests = testGroup "Unit tests"
--  [ testCase "List comparison (different length)" $
--      [1, 2, 3] `compare` [1,2] @?= GT

--  -- the following test does not hold
--  , testCase "List comparison (same length)" $
--      [1, 2, 3] `compare` [1,2,2] @?= LT
--  ]