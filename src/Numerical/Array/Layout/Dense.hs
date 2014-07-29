

-- {-# LANGUAGE PolyKinds   #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-#  LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-#  LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE StandaloneDeriving #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 707
 {-# LANGUAGE AutoDeriveTypeable #-}
#endif

module Numerical.Array.Layout.Dense(
  DenseLayout(..)
  ,Locality(..)
  ,Format(..)
  ,Row
  ,Column
  ,Direct
  ,module Numerical.Array.Address
   ) where



import Numerical.Nat
import Control.Applicative
import Numerical.Array.Address
import Numerical.Array.Locality
import Numerical.Array.Layout.Base
import Numerical.Array.Shape as S
import Data.Data


--import Data.Traversable (Traversable)

import Control.NumericalMonad.State.Strict

import qualified Data.Foldable as F

import Prelude hiding (foldr,foldl,map,scanl,scanr,scanl1,scanr1)


data Direct


data Row


data Column


{-
one important gotcha about shape is that for many formats,
the Shape is the (fmap (+1)) of the largestIndex,
often, but perhaps not always.

-}

{-

need to figure out how to support symmetric and hermitian and triangular
and banded matrices

-}


--class Layout form rank => DenseLayout  form  (rank :: Nat) | form -> rank  where
  {-
  empty class instances for all the dense Layouts
  -}


-- | @'Format' 'Direct' 'Contiguous' ('S' 'Z')@ is a 1dim array 'Layout' with unit stride
data instance Format  Direct Contiguous (S Z) rep  =
    FormatDirectContiguous {
        logicalShapeDirectContiguous :: {-#UNPACK#-} !Int }
    --deriving (Show,Eq,Data)

-- | @'Format' 'Direct' 'Strided'  ('S' 'Z')@ is a 1dim array 'Layout' with a regular stride >= 1
data instance Format  Direct Strided (S Z) rep  =
    FormatDirectStrided {
        logicalShapeDirectStrided :: {-#UNPACK#-}!Int
        ,logicalStrideDirectStrided:: {-#UNPACK#-}!Int}
    --deriving (Show,Eq,Data)

-- | @'Format'  'Row'  'Contiguous' n@ is a rank n Array
data instance  Format  Row  Contiguous n rep   =
    FormatRowContiguous {
        boundsFormRow :: !(Shape n Int)}
    --deriving (Show,Eq,Data)

data instance  Format  Row  Strided n rep  =
    FormatRowStrided
        {boundsFormRowStrided:: !(Shape n Int)
        ,strideFormRowStrided:: !(Shape n Int)}
    --deriving (Show,Eq,Data)

data instance  Format  Row  InnerContiguous n rep  =
    FormatRowInnerContiguous {
        boundsFormRowInnerContig :: !(Shape n Int)
        ,strideFormRowInnerContig:: !(Shape n Int)}
    --deriving (Show,Eq,Data)

data instance  Format  Column Contiguous n  rep =
    FormatColumnContiguous {
      boundsColumnContig :: !(Shape n Int)}
    --deriving (Show,Eq,Data)
--deriving instance (Data (Shape n Int),Typeable n,Typeable rep) =>

data instance  Format Column InnerContiguous n rep  =
    FormatColumnInnerContiguous {
        boundsColumnInnerContig :: !(Shape n Int)
        ,strideFormColumnInnerContig:: !(Shape n Int)
      }

deriving instance Show (Shape n Int) => Show (Format Column InnerContiguous n rep)
deriving instance (Data (Shape n Int),Typeable n,Typeable rep) =>Data (Format Column InnerContiguous n rep)
    --deriving (Show,Eq,Data)

data instance  Format Column Strided n rep  =
    FormatColumnStrided {
      boundsColumnStrided :: !(Shape n Int)
      ,strideFormColumnStrided:: !(Shape n Int)}
deriving instance Show (Shape n Int) => Show (Format Column Strided n rep)
--deriving instance (Eq (Shape n Int)) => Eq (Format Column Strided n rep)
deriving instance (Data (Shape n Int),Typeable n,Typeable rep) => Data (Format Column Strided n rep)
    --deriving (Show,Eq,Data)


type instance Transposed (Format Direct Contiguous (S Z) rep) =
    Format Direct Contiguous (S Z) rep
type instance Transposed (Format Direct Strided (S Z) rep ) =
   Format Direct Strided (S Z) rep

type instance  Transposed (Format Row  Contiguous rank rep) =
  Format Column Contiguous rank rep
type instance Transposed (Format Row  InnerContiguous rank rep) =
    Format Column  InnerContiguous rank rep
type instance  Transposed (Format Row  Strided rank rep) =
    Format Column  Strided rank rep

type instance Transposed (Format Column Contiguous rank rep)=
    Format Row Contiguous rank rep
type instance Transposed (Format Column InnerContiguous rank rep)=
    Format Row  InnerContiguous rank rep
type instance  Transposed (Format Column  Strided rank rep)=
    Format Row  Strided rank rep




-----
-----
-----


instance Layout (Format Direct Contiguous (S Z) rep)  (S Z)  where

    {-# INLINE basicFormShape#-}
    basicFormShape = \ x -> (logicalShapeDirectContiguous x) :* Nil

    transposedLayout = id

    {-# INLINE basicCompareIndex #-}
    basicCompareIndex = \ _  (l:* _) (r:* _) -> compare l r



instance  Layout (Format Direct Strided (S Z) rep)  (S Z)  where

    {-# INLINE basicFormShape #-}
    basicFormShape = \x -> (logicalShapeDirectStrided x) :* Nil

    transposedLayout = id

    {-# INLINE basicCompareIndex #-}
    basicCompareIndex = \ _  (l:* _) (r:* _) -> compare l r

-----
-----
-----


-- strideRow :: Shape rank Int,
instance   (Applicative (Shape rank), Traversable (Shape rank))
    =>  Layout (Format Row  Contiguous rank rep) rank where

    transposedLayout = \(FormatRowContiguous shp) -> FormatColumnContiguous $ reverseShape shp

    {-# INLINE basicFormShape #-}
    basicFormShape = \x -> boundsFormRow x

    {-# INLINE basicCompareIndex #-}
    basicCompareIndex = \ _  ls rs -> foldl majorCompareLeftToRight EQ  $ S.map2 compare ls rs


-----
-----


-- strideRow :: Shape rank Int,
instance   (Applicative (Shape rank), Traversable (Shape rank))
  =>  Layout (Format Row  InnerContiguous rank rep)  rank  where

    {-# INLINE basicFormShape  #-}
    basicFormShape = \x -> boundsFormRowInnerContig x

    transposedLayout = \(FormatRowInnerContiguous shp stride) ->
        FormatColumnInnerContiguous  (reverseShape shp)  (reverseShape stride)

    {-# INLINE basicCompareIndex #-}
    basicCompareIndex = \ _  ls rs ->
      foldl majorCompareLeftToRight EQ  $ S.map2 compare ls rs

---
---

-- strideRow :: Shape rank Int,

instance  (Applicative (Shape rank),Traversable (Shape rank))
  =>  Layout (Format Row  Strided rank rep) rank  where

    {-# INLINE basicFormShape  #-}
    basicFormShape = \x -> boundsFormRowStrided x

    transposedLayout = \(FormatRowStrided shp stride) ->
        FormatColumnStrided  (reverseShape shp)  (reverseShape stride)

    {-# INLINE basicCompareIndex #-}
    basicCompareIndex = \ _  ls rs ->
        foldl majorCompareLeftToRight EQ  $ S.map2 compare ls rs

-----
-----
-----

 -- strideRow :: Shape rank Int,
instance  (Applicative (Shape rank), Traversable (Shape rank))
  =>  Layout (Format Column  Contiguous rank rep)  rank where

    {-# INLINE basicFormShape  #-}
    basicFormShape = \x -> boundsColumnContig x

    transposedLayout = \(FormatColumnContiguous shp)-> FormatRowContiguous $ reverseShape shp

    {-# INLINE basicCompareIndex #-}
    basicCompareIndex = \ _  ls rs -> foldr majorCompareRightToLeft EQ  $ S.map2 compare ls rs


 -- strideRow :: Shape rank Int,
instance  (Applicative (Shape rank), Traversable (Shape rank))
  => Layout (Format Column  InnerContiguous rank rep) rank  where


    {-# INLINE basicFormShape  #-}
    basicFormShape = \x -> boundsColumnInnerContig x

    transposedLayout = \(FormatColumnInnerContiguous shp stride)->
         FormatRowInnerContiguous (reverseShape shp) (reverseShape stride)

    {-# INLINE basicCompareIndex #-}
    basicCompareIndex = \ _  ls rs -> foldr majorCompareRightToLeft EQ  $ S.map2 compare ls rs


 -- strideRow :: Shape rank Int,


instance   (Applicative (Shape rank), Traversable (Shape rank))
  => Layout (Format Column  Strided rank rep) rank where

    {-# INLINE basicFormShape  #-}
    basicFormShape = \x -> boundsColumnStrided x

    transposedLayout = \(FormatColumnStrided shp stride)->
         FormatRowStrided (reverseShape shp) (reverseShape stride)

    {-# INLINE basicCompareIndex #-}
    basicCompareIndex = \ _  ls rs -> foldr majorCompareRightToLeft EQ $ S.map2 compare ls rs


----------------------
----------------------
-----
-----
----------------------
----------------------


class Layout form rank =>  DenseLayout form  (rank :: Nat) | form -> rank  where

-- nb, this should actually be "getCurrentAffineAddressInterval"
    --getCurrentAddressInterval :: form -> Shape rank Int -> UniformAddressInterval

    {-  Not sure if the minAddress and maxAddress
  definitions are correct *in general*, but they're
  correct for the example DenseLayout formats thusfar

    -}

    minAddress :: form  -> Address
    minAddress = \ _ -> Address 0
    {-# INLINE minAddress #-}

    maxAddress :: form -> Address
    maxAddress = \form -> basicToAddress form $ maxIndex form
    {-# INLINE maxAddress #-}
    minIndex :: form -> Shape rank Int
    minIndex = \ _  -> pure 0
    {-# INLINE minIndex #-}
{-
maxIndex assumes:
   basicFormShape form `strictlyDominates` (pure 0)

that is, every axis of a multi dim array, dimension/size must be >=1

FIXME / TODO / AUDIT THIS HARD

 -}
    maxIndex :: form -> Shape rank Int
    maxIndex = \ form -> fmap (flip (-) 1) $  basicFormShape form
    {-# INLINE maxIndex #-}

    basicToAddress :: form  -> Shape rank Int ->   Address


    basicToIndex :: form -> Address -> Shape rank Int


    -- IMPORTANT NOTE, the NextAddress defined via Next Index seems
    -- that it will only be invoked on strided/discontiguous dense formats
    -- this
    basicNextAddress :: form  -> Address ->  Address
    basicNextAddress =  \form shp -> snd
      (basicNextIndex form  $ basicToIndex form  shp )
    {-# INLINE basicNextAddress #-}

    basicNextIndex :: form  -> Shape rank Int ->(Shape rank Int,Address)
    basicNextIndex  = \form shp -> (\ addr ->( basicToIndex form addr, addr) ) $!
       (basicNextAddress form  $ basicToAddress form  shp )
    {-# INLINE  basicNextIndex #-}




    -- one of basicNextAddress and basicNextIndex must always be implemented
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 707
    {-# MINIMAL  basicToIndex, basicToAddress,  (basicNextIndex | basicNextAddress)  #-}
#endif

---
---
---

{-
these are factored out versions of the
various shared computations in both Row and Column Major
rank n Array format computations

-}

{-# INLINE computeStrideShape #-}
computeStrideShape ::
     ((Int -> State Int Int) -> Shape n Int  -> State Int (Shape n Int )) -> Shape n Int -> Shape n Int
computeStrideShape = \trvse shp  ->
    flip evalState 1 $
                      flip  trvse shp  $
                      -- basically accumulating the product of the
                      -- dimensions
                          \ val ->
                               do accum <- get ;
                                  put (val * accum) ;
                                  return accum;




-----
-----
-----

instance DenseLayout (Format Direct Contiguous (S Z) rep)  (S Z)  where


    maxAddress = \ (FormatDirectContiguous ix) -> Address (ix -1)


    {-#INLINE basicToAddress#-}
    basicToAddress   = \ (FormatDirectContiguous _) (j :* _ ) -> Address j

    --basicNextIndex=  undefined -- \ _ x ->  Just $! x + 1
    --note its unchecked!
    {-# INLINE basicToIndex#-}
    basicToIndex =  \ (FormatDirectContiguous _) (Address ix)  -> (ix ) :* Nil

    {-# INLINE basicNextAddress #-}
    basicNextAddress = \ _ addr -> addr + 1





instance DenseLayout (Format Direct Strided (S Z) rep)  (S Z)  where




    {-#INLINE basicToAddress#-}
    basicToAddress   = \ (FormatDirectStrided _ strid) (j :* Nil )->  Address (strid * j)

    {-# INLINE basicNextAddress #-}
    basicNextAddress = \ (FormatDirectStrided _ strid) addr ->  addr + Address strid

    {-# INLINE basicNextIndex#-}
    basicNextIndex =  \ form  (i:* Nil ) ->  (\ix -> (ix,basicToAddress form ix)) $! (i + 1 :* Nil )


    {-# INLINE basicToIndex#-}
    basicToIndex = \ (FormatDirectStrided _ stride) (Address ix)  -> (ix `div` stride ) :* Nil


-----
-----
-----



-- strideRow :: Shape rank Int,
instance   (Applicative (Shape rank),F.Foldable (Shape rank), Traversable (Shape rank))  =>
    DenseLayout (Format Row  Contiguous rank rep) rank where

{-
TODO  AUDIT

-}
    {-# INLINE basicToAddress #-}
    --basicToAddress = \rs tup -> let !strider =takePrefix $! S.scanr (*) 1 (boundsFormRow rs)
    basicToAddress = \rs tup ->
          let !strider =  computeStrideShape (traverse) (boundsFormRow rs)
                  in Address $! S.foldl'  (+) 0 $! map2 (*) strider tup

    {-# INLINE basicNextAddress#-}
    basicNextAddress = \_ addr -> addr + 1

    {-# INLINE basicToIndex #-}
    basicToIndex  =   \ rs (Address ix) ->
        let !striderShape  = computeStrideShape traverse (boundsFormRow rs)

            in
               flip evalState ix $
                  flip (S.backwards traverse)  striderShape $
                  -- want to start from largest stride (which is on the right)
                      \ currentStride ->
                             do remainderIx <- get ;
                                let (!qt,!rm)= quotRem remainderIx currentStride
                                put rm
                                return  qt;




-----
-----

-- strideRow :: Shape rank Int,
instance   (Applicative (Shape rank),F.Foldable (Shape rank), Traversable (Shape rank))
  => DenseLayout (Format Row  InnerContiguous rank rep) rank  where


    {-# INLINE basicToAddress #-}
    basicToAddress = \rs tup ->
                       Address $! S.foldl'  (+) 0 $!
                         map2 (*) (strideFormRowInnerContig rs ) tup

    {-# INLINE basicNextIndex #-}
    basicNextIndex = \ form@(FormatRowInnerContiguous shape _) ix ->
        --S.map snd $!
      (\index -> (index,basicToAddress form  index)) $!
        flip evalState 1 $
           flip traverse  ((,) <$> ix <*> shape) $
              \(ixv ,shpv   )->
                  do  carry <-get
                      let (newCarry,modVal)=divMod (carry + ixv) shpv
                      put newCarry
                      return modVal


    {-# INLINE basicToIndex #-}
    basicToIndex  =   \ rs (Address ix) ->   flip evalState ix $
                          flip ( S.backwards traverse)  (strideFormRowInnerContig rs ) $
                              \ currentStride ->
                                     do remainderIx <- get ;
                                        let (!qt,!rm)= quotRem remainderIx currentStride
                                        put rm
                                        return  qt;



---
---
-- strideRow :: Shape rank Int,

instance  (Applicative (Shape rank),F.Foldable (Shape rank), Traversable (Shape rank))
  => DenseLayout (Format Row  Strided rank rep) rank  where



    {-# INLINE basicToAddress #-}
    basicToAddress = \rs tup ->   Address $!
          S.foldl'  (+) 0 $! map2 (*) (strideFormRowStrided rs ) tup

    {-# INLINE basicNextIndex #-}
    basicNextIndex = \ form@(FormatRowStrided shape _) ix ->
      (\index -> (index,basicToAddress form index)) $!
        flip evalState 1 $
           flip traverse  ((,) <$> ix <*> shape) $
              \(ixv ,shpv   )->
                  do  carry <-get
                      let (newCarry,modVal)=divMod (carry + ixv) shpv
                      put newCarry
                      return modVal


    {-# INLINE basicToIndex #-}
    basicToIndex  =   \ rs (Address ix) ->   flip evalState ix $
                          flip (S.backwards traverse ) (strideFormRowStrided rs ) $
                              \ currentStride ->
                                     do remainderIx <- get ;
                                        let (!qt,!rm)= quotRem remainderIx currentStride
                                        put rm
                                        return  qt;




-----
-----
-----


 -- strideRow :: Shape rank Int,
instance  (Applicative (Shape rank),F.Foldable (Shape rank), Traversable (Shape rank))
  => DenseLayout (Format Column  Contiguous rank rep)  rank where



    {-# INLINE basicToAddress #-}
    basicToAddress = \rs tup ->
          let !strider = computeStrideShape  (S.backwards traverse) (boundsColumnContig rs)
                                in Address $! S.foldl'  (+) 0 $! map2 (*) strider tup

    {-# INLINE basicNextAddress#-}
    basicNextAddress = \_ addr -> addr + 1

    {-# INLINE basicToIndex #-}
    basicToIndex  =   \ rs (Address ix) ->
            let !striderShape  =  computeStrideShape  (S.backwards traverse) (boundsColumnContig rs)
                in
                   flip evalState ix $
                          flip  traverse  striderShape $
                              \ currentStride ->
                                     do remainderIx <- get ;
                                        let (!qt,!rm)= quotRem remainderIx currentStride
                                        put rm
                                        return  qt;





 -- strideRow :: Shape rank Int,
instance  (Applicative (Shape rank),F.Foldable (Shape rank), Traversable (Shape rank))
  => DenseLayout (Format Column  InnerContiguous rank rep) rank  where


    {-# INLINE basicToAddress #-}
    basicToAddress    =   \ form tup -> let !strider =   strideFormColumnInnerContig form
                                in Address $! foldl' (+) 0  $! map2 (*) strider tup
    {-# INLINE basicNextIndex #-}
    basicNextIndex = \ form@(FormatColumnInnerContiguous shape _) ix ->
        --S.map snd $!
      (\index -> (index,basicToAddress form index)) $!
        flip evalState 1 $
           flip (S.backwards traverse)  ((,) <$> ix <*> shape) $
              \(ixv ,shpv   )->
                  do  carry <-get
                      let (newCarry,modVal)=divMod (carry + ixv) shpv
                      put newCarry
                      return modVal


    {-# INLINE basicToIndex #-}
    basicToIndex  =   \ rs (Address ix) ->   flip evalState ix $
                          flip S.traverse  (strideFormColumnInnerContig rs ) $
                              \ currentStride ->
                                     do remainderIx <- get ;
                                        let (!qt,!rm)= quotRem remainderIx currentStride
                                        put rm
                                        return  qt;




instance   (Applicative (Shape rank),F.Foldable (Shape rank), Traversable (Shape rank))
  => DenseLayout (Format Column  Strided rank rep) rank where

    {-# INLINE basicToAddress #-}
    basicToAddress    =   \ form tup -> let !strider =   strideFormColumnStrided form
                                in Address $! foldl' (+) 0  $! map2 (*) strider tup

    {-# INLINE basicNextIndex #-}
    basicNextIndex = \ form@(FormatColumnStrided shape _) ix ->
        --S.map snd $!
      (\index -> (index,basicToAddress form index)) $!
        flip evalState 1 $
           flip (S.backwards traverse)  ((,) <$> ix <*> shape) $
              \(ixv ,shpv   )->
                  do  carry <-get
                      let (newCarry,modVal)=divMod (carry + ixv) shpv
                      put newCarry
                      return modVal


    {-# INLINE basicToIndex #-}
    basicToIndex  =   \ rs (Address ix) ->   flip evalState ix $
                          flip S.traverse  (strideFormColumnStrided rs ) $
                              \ currentStride ->
                                     do remainderIx <- get ;
                                        let (!qt,!rm)= quotRem remainderIx currentStride
                                        put rm
                                        return  qt;






{-
*Numerical.Array.Layout> basicToAddress (FormColumn (2 :* 3 :* 7 :* Nil)) (0:* 2 :* 2 :* Nil)
Address 16
*Numerical.Array.Layout> basicToAddress (FormColumn (2 :* 3 :* 7 :* Nil)) (1:* 0 :* 0 :* Nil)
Address 1
*Numerical.Array.Layout> basicToAddress (FormColumn (2 :* 3 :* 7 :* Nil)) (0:* 0 :* 0 :* Nil)
Address 0
*Numerical.Array.Layout> basicToAddress (FormColumn (2 :* 3 :* 7 :* Nil)) (0:* 1 :* 0 :* Nil)
Address 2
*Numerical.Array.Layout> basicToAddress (FormColumn (2 :* 3 :* 7 :* Nil)) (0:* 0 :* 1 :* Nil)
-}


