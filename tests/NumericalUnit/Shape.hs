{-# LANGUAGE DataKinds, GADTs, TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}

module NumericalUnit.Shape(unitTestShape) where


import Numerical.Array.Shape as S
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Unboxed as UV
import Prelude as P
import Test.Hspec.Expectations
import Test.Hspec

unitTestShape :: Spec
unitTestShape = describe "unit tests for Shape" $ do
        specify "foldl on shape" $  S.foldl (+) 0 (1:* 2:* 3 :* Nil )  `shouldBe`   P.foldl   (+) 0  [1,2,3]
        specify "foldl' on shape" $ S.foldl' (+) 0 (1:* 2:* 3 :* Nil )  `shouldBe`   P.foldl (+) 0  [1,2,3]
        specify "foldr on shape" $  S.foldr (+) 0 (1:* 2:* 3 :* Nil )  `shouldBe`   P.foldr  (+) 0  [1,2,3]
        specify "foldl1 on shape" $  S.foldl1 (+) (1:* 2:* 3 :* Nil )  `shouldBe`   P.foldl1   (+)  [1,2,3]
        specify "foldr1 on shape" $  S.foldr1 (+) (1:* 2:* 3 :* Nil )  `shouldBe`   P.foldr1  (+)  [1,2,3]

        specify "== on Nil" $ Nil == Nil `shouldBe` True
        specify "== on identical shapes" $ (1:* 2:* 3 :*  Nil) == (1:* 2:* 3 :*  Nil) `shouldBe` True
        specify "== on different shapes" $ (1:* 2:* 3 :*  Nil) == (1:* 2:* 2 :*  Nil) `shouldBe` False

        specify "fmap on Nil" $ fmap (+1) Nil `shouldBe` Nil
        specify "fmap on size n shape" $ fmap (+1) (1 :* 3 :* Nil) `shouldBe` (2:* 4 :* Nil)

        specify "pure on Nil" $ pure Nil `shouldBe` Nil
        specify "pure on size 1" $ (pure 1 :: Shape ('S 'Z) Int) `shouldBe` (1 :* Nil)

        specify "<*> with Nil should always be Nil" $ Nil <*> Nil `shouldBe` Nil
        specify "<*> with size 1 shape" $ (+) <$> (1 :* Nil) <*> (2 :* Nil) `shouldBe` 3 :* Nil

        specify "traverse on Nil " $ traverse Just Nil `shouldBe` Just Nil
        specify "traverse on size 1 shape" $ traverse (Just) (1 :* Nil) `shouldBe` Just (1:* Nil)
        specify "traverse on size n shape" $ traverse (Just) (1 :* 2 :* Nil) `shouldBe` Just (1 :* 2 :* Nil)

        specify "sequenceA on Nil" $ sequenceA Nil `shouldBe` Nil
        specify "sequenceA on size 1 shape" $ sequenceA (Just 1 :* Nil) `shouldBe` Just (1 :* Nil)
        specify "sequenceA on size n shape" $ sequenceA (Just 1 :* Just 2 :* Nil) `shouldBe` Just (1 :* 2:*  Nil)

        specify "mapM on Nil" $ mapM (Just) Nil `shouldBe` Just Nil
        specify "mapM on size 1 shape" $ mapM (Just) (1 :* Nil) `shouldBe` Just (1 :* Nil)
        specify "mapM on size n shape" $ mapM (Just) (1:* 2 :* Nil) `shouldBe` Just (1:* 2 :* Nil)

        specify "sequence on Nil" $ do a <- sequence Nil ; a `shouldBe` Nil
        specify "sequence on Size 1 shape" $ sequence (Just 1 :* Nil) `shouldBe` Just (1 :* Nil)
        specify "sequence on Size n shape" $ sequence (Just 1 :* Just 2 :* Nil) `shouldBe` Just (1 :* 2 :* Nil)

        specify "map on shape" $ S.map (+1) (1:* 2:* 3 :* Nil) `shouldBe` (2:* 3:* 4 :* Nil)
        specify "map2 on shape" $ S.map2 (+) (1:* 2:* 3 :* Nil) (2:* 3:* 4 :* Nil) `shouldBe` (3:* 5:* 7 :* Nil)

        specify "shapeToList on shape" $ shapeToList (1:* 2:* 3 :*  Nil) `shouldBe` [1, 2, 3]
        specify "shapeSize on shape" $ shapeSize (1:* 2:* 3 :* Nil)  `shouldBe` length (shapeToList (1:* 2:* 3 :* Nil))

        specify "backwards traverse on shape" $ backwards traverse (:[]) (1:* 2 :* Nil) `shouldBe` [(1:* 2 :* Nil)]

        specify "unShapeVector on shape" $
          do a <- return (uvFromList [1:* 2:* Nil,2:* 3:* Nil,3:* 4:* Nil :: Shape (S (S Z)) Int])
             unShapeVector a `shouldBe` (3, uvFromList [1,2,3] :* uvFromList [2,3,4] :* Nil)

        specify "reShapeVector on shape" $
          do a <- return (uvFromList [1:*Nil,2:*Nil :: Shape (S Z) Int])
             reShapeVector (2, uvFromList [1,2] :* Nil) `shouldBe` a

        specify "shapeToList on shape" $  S.shapeToList (1:* 2 :* 3 :* Nil) `shouldBe` [1,2,3]

        specify "Show on Nil shape" $ show Nil `shouldBe` "Nil"
        specify "Show on 1:* Nil"  $ show (1:* Nil) `shouldBe` "1 :* Nil"

        specify "storable on size 0 shape" $
          do a <- return (svFromList [Nil,Nil :: Shape Z Int]) ; SV.toList a `shouldBe` [Nil,Nil]
        specify "storable on size 1 shape" $
          do a <- return (svFromList [1:*Nil,2:*Nil :: Shape (S Z) Int]) ; SV.toList a `shouldBe` [1:*Nil,2:*Nil]
        specify "storable on size 2 shape" $
          do  a <- return (svFromList [3:* 4:* Nil,1:*2:*Nil :: Shape (S (S Z)) Int]) ;
              SV.toList a `shouldBe` [3:* 4:* Nil,1:*2:*Nil]

        specify "unboxed on size 0 shape" $
          do a <- return (uvFromList [Nil,Nil :: Shape Z Int]) ; UV.toList a `shouldBe` [Nil,Nil]
        specify "unboxed on size 1 shape" $
          do a <- return (uvFromList [1:*Nil,2:*Nil :: Shape (S Z) Int]) ; UV.toList a `shouldBe` [1:*Nil,2:*Nil]
        specify "unboxed on size 2 shape" $
          do  a <- return (uvFromList [3:* 4:* Nil,1:*2:*Nil :: Shape (S (S Z)) Int]) ;
                UV.toList a `shouldBe` [3:* 4:* Nil,1:*2:*Nil]

        specify "weaklyDominates equal to" $ (1:* 2:* 3 :* Nil ) `weaklyDominates` (1:* 2:* 3 :* Nil ) `shouldBe` True
        specify "weaklyDominates less than" $ (1:* 2:* 3 :* Nil ) `weaklyDominates` (1:* 2:* 2 :* Nil ) `shouldBe` True
        specify "weaklyDominates greater than" $ (1:* 3 :* Nil ) `weaklyDominates` (2:* 3 :* Nil ) `shouldBe` False
    
        specify "strictlyDominates equal to" $ (1:* 2:* 3 :* Nil ) `strictlyDominates` (1:* 2:* 3 :* Nil ) `shouldBe` False
        specify "strictlyDominates less than" $ (2:* 3:* 4 :* Nil ) `strictlyDominates` (1:* 2:* 3 :* Nil ) `shouldBe` True
        specify "strictlyDominates greater than" $ (2:* 2:* 3 :* Nil ) `strictlyDominates` (1:* 3:* 3 :* Nil ) `shouldBe` False
    
        specify "reverseShape on size 0 shape" $ reverseShape Nil `shouldBe` Nil
        specify "reverseShape on size 1 shape" $ reverseShape (1:* Nil) `shouldBe` (1:* Nil)
        specify "reverseShape on size 2 shape" $ reverseShape (1:* 2:* Nil) `shouldBe` (2:* 1:* Nil)
        specify "reverseShape on size 3 shape" $ reverseShape (1:* 2:* 3:* Nil) `shouldBe` (3:* 2:* 1:* Nil)
        specify "reverseShape on size >3 shape" $ reverseShape (1:* 2:* 3:* 4:* 5:* Nil) `shouldBe` (5:* 4:* 3:* 2:* 1:* Nil)

    where
        {- The NOINLINE is need to properly check storable /unboxed instances, otherwise fusion removes the allocation! -}
        svFromList  :: SV.Storable a => [a] -> SV.Vector a
        svFromList = SV.fromList
        {-# NOINLINE svFromList #-}

        uvFromList :: UV.Unbox a => [a] -> UV.Vector a
        uvFromList = UV.fromList
        {-# NOINLINE uvFromList#-}
