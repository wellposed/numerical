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
    [ testCase "foldl on shape" $  S.foldl (+) 0 (1:* 2:* 3 :* Nil )  @?=   P.foldl   (+) 0  [1,2,3]  
    , testCase "foldr on shape" $  S.foldr (+) 0 (1:* 2:* 3 :* Nil )  @?=   P.foldr  (+) 0  [1,2,3]  
    , testCase "scanr1 on shape" $ S.scanr1 (+) 0 (1:* 1 :* 1:* Nil )   @?=  (3:* 2:* 1 :* Nil ) 
    , testCase "scanl1 on shape" $ S.scanl1 (+) 0 (1:* 1 :* 1:* Nil )   @?=  (1:* 2:* 3:* Nil ) 
    , testCase "scanr1 on shape" $ S.scanr (+) 0 (1:* 1 :* 1:* Nil )   @?=  (3:* 2:* 1 :* 0:* Nil ) 
    , testCase "scanl1 on shape" $ S.scanl (+) 0 (1:* 1 :* 1:* Nil )   @?=  (0 :* 1:* 2:* 3:* Nil )     
    , testCase "shapeToList on shape" $  S.shapeToList (1:* 2 :* 3 :* Nil) @?= [1,2,3]  
    , testCase "unsnoc on shape " $ (snd . S.unsnoc) (1:*2 :* Nil ) @?= 2 
    , testCase "snoc cons  uncons" $ (S.shapeToList . flip  snoc 1 . cons 1 )  Nil  @?= [1,1]
    , testCase "foldl1 on shape" $  S.foldl1 (+) (1:* 2:* 3 :* Nil )  @?=   P.foldl1   (+)  [1,2,3]  
    , testCase "foldr1 on shape" $  S.foldr1 (+) (1:* 2:* 3 :* Nil )  @?=   P.foldr1  (+)  [1,2,3]  
    , testCase "Show on Nil shape" $ show Nil @?= "Nil"
    , testCase "Show on 1:* Nil"  $ show (1:* Nil) @?= "1 :* Nil" 

    ]