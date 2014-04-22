-- {-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds, GADTs, TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}

module NumericalUnit.Shape(unitTestShape) where 

import Test.HUnit
import Numerical.Array.Shape as S 
import Prelude as P
import Test.Tasty
import Test.Tasty.HUnit

unitTestShape = testGroup "Shape Unit tests"
    [ testCase "foldl on shape" $ ( S.foldl (+) 0 (1:* 2:* 3 :* Nil )  @?=  ( P.foldl   (+) 0  [1,2,3])  )


    ]