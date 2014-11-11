module Main where


import   NumericalUnit.Layout
import   NumericalUnit.Shape



import Data.List
import Data.Ord

import Test.Hspec
import Control.Exception (evaluate)

main :: IO ()
main = hspec  $ do
  describe "Shape Unit Tests" $  unitTestShape

--main = defaultMain tests

--tests :: Spec
--tests = testGroup "Unit Tests" [unitTestShape] -- , unitTestLayout ]


--unitTests = testGroup "Unit tests"
--  [ testCase "List comparison (different length)" $
--      [1, 2, 3] `compare` [1,2] @?= GT

--  -- the following test does not hold
--  , testCase "List comparison (same length)" $
--      [1, 2, 3] `compare` [1,2,2] @?= LT
--  ]
