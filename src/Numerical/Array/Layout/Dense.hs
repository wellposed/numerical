

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
  ,module Numerical.Array.Layout.Base
   ) where



import Numerical.Nat
import Control.Applicative
import Numerical.Array.Locality
import Numerical.Array.Layout.Base
import Numerical.Array.Shape as S
import Data.Data(Data,Typeable)


--import Data.Traversable (Traversable)

import Control.NumericalMonad.State.Strict

import qualified Data.Foldable as F
import Data.Traversable

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
data instance Format  Direct 'Contiguous ('S 'Z) rep  =
    FormatDirectContiguous {
        logicalShapeDirectContiguous :: {-#UNPACK#-} !Int }
    deriving (Show,Eq,Data)

-- | @'Format' 'Direct' 'Strided'  ('S' 'Z')@ is a 1dim array 'Layout' with a regular stride >= 1
data instance Format  Direct 'Strided ('S 'Z) rep  =
    FormatDirectStrided {
        logicalShapeDirectStrided :: {-#UNPACK#-}!Int
        ,logicalStrideDirectStrided:: {-#UNPACK#-}!Int}
    --deriving (Show,Eq,Data)

-- | @'Format'  'Row'  'Contiguous' n@ is a rank n Array
data instance  Format  Row  'Contiguous n rep   =
    FormatRowContiguous {
        boundsFormRow :: !(Shape n Int)}
    --deriving (Show,Eq,Data)

data instance  Format  Row  'Strided n rep  =
    FormatRowStrided
        {boundsFormRowStrided:: !(Shape n Int)
        ,strideFormRowStrided:: !(Shape n Int)}
    --deriving (Show,Eq,Data)

data instance  Format  Row  'InnerContiguous n rep  =
    FormatRowInnerContiguous {
        boundsFormRowInnerContig :: !(Shape n Int)
        ,strideFormRowInnerContig:: !(Shape n Int)}
    --deriving (Show,Eq,Data)

data instance  Format  Column 'Contiguous n  rep =
    FormatColumnContiguous {
      boundsColumnContig :: !(Shape n Int)}
    --deriving (Show,Eq,Data)
--deriving instance (Data (Shape n Int),Typeable n,Typeable rep) =>

data instance  Format Column 'InnerContiguous n rep  =
    FormatColumnInnerContiguous {
        boundsColumnInnerContig :: !(Shape n Int)
        ,strideFormColumnInnerContig:: !(Shape n Int)
      }

deriving instance Show (Shape n Int) => Show (Format Column 'InnerContiguous n rep)
deriving instance (Data (Shape n Int),Typeable n,Typeable rep) =>Data (Format Column 'InnerContiguous n rep)
    --deriving (Show,Eq,Data)

data instance  Format Column 'Strided n rep  =
    FormatColumnStrided {
      boundsColumnStrided :: !(Shape n Int)
      ,strideFormColumnStrided:: !(Shape n Int)}
deriving instance Show (Shape n Int) => Show (Format Column 'Strided n rep)
--deriving instance (Eq (Shape n Int)) => Eq (Format Column Strided n rep)
deriving instance (Data (Shape n Int),Typeable n,Typeable rep) => Data (Format Column 'Strided n rep)
    --deriving (Show,Eq,Data)


type instance Transposed (Format Direct 'Contiguous ('S 'Z) rep) =
    Format Direct 'Contiguous ('S 'Z) rep
type instance Transposed (Format Direct 'Strided ('S 'Z) rep ) =
   Format Direct 'Strided ('S 'Z) rep

type instance  Transposed (Format Row  'Contiguous rank rep) =
  Format Column 'Contiguous rank rep
type instance Transposed (Format Row  'InnerContiguous rank rep) =
    Format Column  'InnerContiguous rank rep
type instance  Transposed (Format Row  'Strided rank rep) =
    Format Column  'Strided rank rep

type instance Transposed (Format Column 'Contiguous rank rep)=
    Format Row 'Contiguous rank rep
type instance Transposed (Format Column 'InnerContiguous rank rep)=
    Format Row  'InnerContiguous rank rep
type instance  Transposed (Format Column  'Strided rank rep)=
    Format Row  'Strided rank rep


{-
a bunch of routines used to give various Layout operations for
array Formats that have  DenseLayout instance
not exported or for human use
-}
{-# INLINE basicAddressRangeGeneric #-}
basicAddressRangeGeneric ::
  (Functor (Shape rank),Applicative (Shape rank),F.Foldable (Shape rank),
            DenseLayout form rank, Address~LayoutAddress form)=> form -> Maybe (Range Address)
basicAddressRangeGeneric = \ form ->
  if  (fmap (flip (-) 1)$ basicLogicalShape form) `strictlyDominates`  pure 0
    then Just $!
       Range  (basicToDenseAddress form  $! pure 0)
              (basicToDenseAddress form $!
                  fmap (flip (-) 1) $! basicLogicalShape form)
    else Nothing

{-# INLINE basicToAddressDenseGeneric #-}
basicToAddressDenseGeneric :: (Functor (Shape rank),Applicative (Shape rank),F.Foldable (Shape rank),
    DenseLayout form rank,Address~LayoutAddress form) => form -> Shape rank Int -> Maybe Address
basicToAddressDenseGeneric = \ form ix ->
  if (fmap (flip (-) 1)$ basicLogicalShape form) `weaklyDominates`  ix
    && ix `weaklyDominates` pure 0
    then Just $ basicToDenseAddress form ix
    else Nothing
{-# INLINE basicToIndexDenseGeneric #-}
basicToIndexDenseGeneric ::
  (Functor (Shape rank),F.Foldable (Shape rank),
    DenseLayout form rank,Address~LayoutAddress form) =>  form -> Address -> Shape rank Int
basicToIndexDenseGeneric = \form addr ->
  basicToDenseIndex form addr

{-# INLINE basicNextAddressDenseGeneric #-}
basicNextAddressDenseGeneric ::
  (Functor (Shape rank),F.Foldable (Shape rank),
    DenseLayout form rank,Address~LayoutAddress form) =>  form -> Address-> Maybe Address
basicNextAddressDenseGeneric= \ form addy ->
  case  basicAddressRange form of
    Just  (Range lo hi ) ->  if addy >= lo && addy < hi
        then Just $! basicNextDenseAddress form addy
        else Nothing
    Nothing -> Nothing

{-# INLINE basicNextIndexDenseGeneric #-}
basicNextIndexDenseGeneric :: (Functor (Shape rank),F.Foldable (Shape rank),Applicative (Shape rank),
    DenseLayout form rank,Address~LayoutAddress form)  =>
    form -> Shape rank Int -> Maybe Address ->Maybe (Shape rank Int,Address)
basicNextIndexDenseGeneric= \form ix _  ->
  if (fmap (flip (-) 1)$ basicLogicalShape form) `strictlyDominates`  ix
      && ix `weaklyDominates` pure 0
    then
      Just $! basicNextDenseIndex form ix
    else
      Nothing
{-
{- | note that basicAffineAddressShiftGeneric may be suboptimal,
need to investigate what the core looks like -}
{-# INLINE basicAffineAddressShiftDenseGeneric #-}
basicAffineAddressShiftDenseGeneric :: (DenseLayout form rank, Address ~ LayoutAddress form)=>
  form -> Address -> Int -> Maybe Address
basicAffineAddressShiftDenseGeneric form addy shift = do
    newForm <- return $ basicFormContiguification form
    nativeIndex <- return $ basicToDenseIndex form addy
    ----- NOOOOO, this actually needs the "contiguifiction"
    ----- its broken if we internally do a transponse between row and column
    --- because this
    popBaseAddress <- return $   basicToDenseAddress newForm nativeIndex
    rng <- basicAddressRange newForm
    candidateAddress <- return $ addy + Address shift
    if getConst $ rangeMin (const . Const) rng <= candidateAddress
        && candidateAddress  <=
-}



-----
-----
-----


type instance LayoutAddress (Format Direct 'Contiguous ('S 'Z) rep) = Address
type instance LayoutLogicalFormat  (Format Direct 'Contiguous ('S 'Z) rep) = Format Direct 'Contiguous ('S 'Z) rep
instance Layout (Format Direct 'Contiguous ('S 'Z) rep)  ('S 'Z)  where



    {-# INLINE basicLogicalShape #-}
    basicLogicalShape = \ x -> (logicalShapeDirectContiguous x) :* Nil

    basicLogicalForm = id

    transposedLayout = id

    {-# INLINE basicCompareIndex #-}
    basicCompareIndex = \ _  (l:* _) (r:* _) -> compare l r

    basicAddressRange =  basicAddressRangeGeneric

    indexToAddress = basicToAddressDenseGeneric

    addressToIndex = basicToIndexDenseGeneric


    basicNextAddress=basicNextAddressDenseGeneric

    basicNextIndex= basicNextIndexDenseGeneric


    basicAddressPopCount = \ _   (Range (Address lo) (Address hi )) ->
      if  hi >= lo then hi - lo
        else error $ "for basicAddressPopCount requires address obey hi >= lo, given: "
          ++ show hi ++ " "  ++ show lo
      -- FIX me, add the range error checking
      -- in the style of the Sparse instances


    basicAddressAsInt = \ _ (Address a) -> a


    {-# INLINE basicAddressRange #-}
    {-# INLINE indexToAddress #-}
    {-# INLINE addressToIndex #-}
    {-# INLINE basicNextAddress #-}
    {-# INLINE basicNextIndex #-}
    {-# INLINE basicAddressPopCount #-}

type instance LayoutAddress (Format Direct 'Strided ('S 'Z) rep) = Address
instance  Layout (Format Direct 'Strided ('S 'Z) rep)  ('S 'Z)  where

    {-# INLINE basicLogicalShape #-}
    basicLogicalShape = \x -> (logicalShapeDirectStrided x) :* Nil

    transposedLayout = id

    {-# INLINE basicCompareIndex #-}
    basicCompareIndex = \ _  (l:* _) (r:* _) -> compare l r

    basicAddressRange = basicAddressRangeGeneric

    indexToAddress = basicToAddressDenseGeneric

    addressToIndex = basicToIndexDenseGeneric

    basicNextAddress=  basicNextAddressDenseGeneric

    basicNextIndex=  basicNextIndexDenseGeneric

    basicAddressPopCount = \form@(FormatDirectStrided size _ ) (Range loA hiA)->
      let newForm = (FormatDirectContiguous size)
        in
          basicAddressPopCount  newForm
            (Range (basicToDenseAddress newForm $ basicToDenseIndex form loA)
                 (basicToDenseAddress newForm $ basicToDenseIndex form hiA) )

    basicAddressAsInt = \ _ (Address a) -> a

    {-# INLINE basicAddressRange #-}
    {-# INLINE indexToAddress #-}
    {-# INLINE addressToIndex #-}
    {-# INLINE basicNextAddress #-}
    {-# INLINE basicNextIndex #-}
    {-# INLINE basicAddressPopCount #-}


-- one type family instance for all the rows
type instance LayoutAddress (Format Row locality    rank rep) = Address

instance   (Applicative (Shape rank), Traversable (Shape rank))
    =>  Layout (Format Row  'Contiguous rank rep) rank where

    transposedLayout = \(FormatRowContiguous shp) -> FormatColumnContiguous $ reverseShape shp

    {-# INLINE basicLogicalShape #-}
    basicLogicalShape =  boundsFormRow

    {-# INLINE basicCompareIndex #-}
    basicCompareIndex = \ _  ls rs -> foldl majorCompareLeftToRight EQ  $ S.map2 compare ls rs

    basicAddressPopCount = \ _   (Range (Address lo) (Address hi )) -> hi - lo
      -- FIX me, add the range error checking
      -- in the style of the Sparse instances
    basicAddressRange = basicAddressRangeGeneric

    indexToAddress = basicToAddressDenseGeneric

    addressToIndex = basicToIndexDenseGeneric

    basicNextAddress=  basicNextAddressDenseGeneric

    basicNextIndex=  basicNextIndexDenseGeneric

    basicAddressAsInt = \ _ (Address a) -> a

    {-# INLINE basicAddressRange #-}
    {-# INLINE indexToAddress #-}
    {-# INLINE addressToIndex #-}
    {-# INLINE basicNextAddress #-}
    {-# INLINE basicNextIndex #-}
    {-# INLINE basicAddressPopCount #-}



instance   (Applicative (Shape rank), Traversable (Shape rank))
  =>  Layout (Format Row  'InnerContiguous rank rep)  rank  where

    {-# INLINE basicLogicalShape  #-}
    basicLogicalShape = boundsFormRowInnerContig

    transposedLayout = \(FormatRowInnerContiguous shp stride) ->
        FormatColumnInnerContiguous  (reverseShape shp)  (reverseShape stride)

    {-# INLINE basicCompareIndex #-}
    basicCompareIndex = \ _  ls rs ->
      foldl majorCompareLeftToRight EQ  $ S.map2 compare ls rs

    basicAddressRange = basicAddressRangeGeneric

    indexToAddress = basicToAddressDenseGeneric

    addressToIndex = basicToIndexDenseGeneric

    basicNextAddress=  basicNextAddressDenseGeneric

    basicNextIndex=  basicNextIndexDenseGeneric

    basicAddressPopCount = \form@(FormatRowInnerContiguous size _) (Range loA hiA)->
      let newForm = (FormatRowContiguous size)
        in
          basicAddressPopCount  newForm
            (Range (basicToDenseAddress newForm $ basicToDenseIndex form loA)
                 (basicToDenseAddress newForm $ basicToDenseIndex form hiA) )

    basicAddressAsInt = \ _ (Address a) -> a

    {-# INLINE basicAddressRange #-}
    {-# INLINE indexToAddress #-}
    {-# INLINE addressToIndex #-}
    {-# INLINE basicNextAddress #-}
    {-# INLINE basicNextIndex #-}
    {-# INLINE basicAddressPopCount #-}



instance  (Applicative (Shape rank),Traversable (Shape rank))
  =>  Layout (Format Row 'Strided rank rep) rank  where

    {-# INLINE basicLogicalShape  #-}
    basicLogicalShape =  boundsFormRowStrided

    transposedLayout = \(FormatRowStrided shp stride) ->
        FormatColumnStrided  (reverseShape shp)  (reverseShape stride)

    {-# INLINE basicCompareIndex #-}
    basicCompareIndex = \ _  ls rs ->
        foldl majorCompareLeftToRight EQ  $ S.map2 compare ls rs

    basicAddressRange = basicAddressRangeGeneric

    indexToAddress = basicToAddressDenseGeneric

    addressToIndex = basicToIndexDenseGeneric

    basicNextAddress=  basicNextAddressDenseGeneric

    basicNextIndex=  basicNextIndexDenseGeneric

    basicAddressPopCount = \form@(FormatRowStrided size _) (Range loA hiA)->
      let newForm = (FormatRowContiguous size)
        in
          basicAddressPopCount  newForm
            (Range (basicToDenseAddress newForm $ basicToDenseIndex form loA)
                 (basicToDenseAddress newForm $ basicToDenseIndex form hiA) )

    basicAddressAsInt = \ _ (Address a) -> a

    {-# INLINE basicAddressRange #-}
    {-# INLINE indexToAddress #-}
    {-# INLINE addressToIndex #-}
    {-# INLINE basicNextAddress #-}
    {-# INLINE basicNextIndex #-}
    {-# INLINE basicAddressPopCount #-}


type instance LayoutAddress (Format Column locality    rank rep) = Address
instance  (Applicative (Shape rank), Traversable (Shape rank))
  =>  Layout (Format Column 'Contiguous rank rep)  rank where

    {-# INLINE basicLogicalShape  #-}
    basicLogicalShape =  boundsColumnContig

    transposedLayout = \(FormatColumnContiguous shp)-> FormatRowContiguous $ reverseShape shp

    {-# INLINE basicCompareIndex #-}
    basicCompareIndex = \ _  ls rs -> foldr majorCompareRightToLeft EQ  $ S.map2 compare ls rs

    basicAddressPopCount = \ _   (Range (Address lo) (Address hi )) ->
        if hi >= lo then hi - lo
            else  error  $ "for basicAddressPopCount, require address hi >= lo, given: "
              ++ show hi ++ " " ++ show lo
      -- FIX me, add the range error checking
      -- in the style of the Sparse instances
    basicAddressRange = basicAddressRangeGeneric

    indexToAddress = basicToAddressDenseGeneric

    addressToIndex = basicToIndexDenseGeneric

    basicNextAddress=  basicNextAddressDenseGeneric

    basicNextIndex=  basicNextIndexDenseGeneric

    basicAddressAsInt = \ _ (Address a) -> a

    {-# INLINE basicAddressRange #-}
    {-# INLINE indexToAddress #-}
    {-# INLINE addressToIndex #-}
    {-# INLINE basicNextAddress #-}
    {-# INLINE basicNextIndex #-}
    {-# INLINE basicAddressPopCount #-}


instance  (Applicative (Shape rank), Traversable (Shape rank))
  => Layout (Format Column 'InnerContiguous rank rep) rank  where


    {-# INLINE basicLogicalShape  #-}
    basicLogicalShape =  boundsColumnInnerContig

    transposedLayout = \(FormatColumnInnerContiguous shp stride)->
         FormatRowInnerContiguous (reverseShape shp) (reverseShape stride)

    {-# INLINE basicCompareIndex #-}
    basicCompareIndex = \ _  ls rs -> foldr majorCompareRightToLeft EQ  $ S.map2 compare ls rs

    basicAddressRange = basicAddressRangeGeneric

    indexToAddress = basicToAddressDenseGeneric

    addressToIndex = basicToIndexDenseGeneric

    basicNextAddress=  basicNextAddressDenseGeneric

    basicNextIndex=  basicNextIndexDenseGeneric

    basicAddressPopCount = \form@(FormatColumnInnerContiguous size _) (Range loA hiA)->
      let newForm = (FormatColumnContiguous size)
        in
          basicAddressPopCount  newForm
            (Range (basicToDenseAddress newForm $ basicToDenseIndex form loA)
                 (basicToDenseAddress newForm $ basicToDenseIndex form hiA) )

    basicAddressAsInt = \ _ (Address a) -> a
 -- strideRow :: Shape rank Int,

    {-# INLINE basicAddressRange #-}
    {-# INLINE indexToAddress #-}
    {-# INLINE addressToIndex #-}
    {-# INLINE basicNextAddress #-}
    {-# INLINE basicNextIndex #-}
    {-# INLINE basicAddressPopCount #-}

instance   (Applicative (Shape rank), Traversable (Shape rank))
  => Layout (Format Column 'Strided rank rep) rank where

    {-# INLINE basicLogicalShape  #-}
    basicLogicalShape = boundsColumnStrided

    transposedLayout = \(FormatColumnStrided shp stride)->
         FormatRowStrided (reverseShape shp) (reverseShape stride)

    {-# INLINE basicCompareIndex #-}
    basicCompareIndex = \ _  ls rs -> foldr majorCompareRightToLeft EQ $ S.map2 compare ls rs

    basicAddressRange = basicAddressRangeGeneric

    indexToAddress = basicToAddressDenseGeneric

    addressToIndex = basicToIndexDenseGeneric

    basicNextAddress=  basicNextAddressDenseGeneric

    basicNextIndex=  basicNextIndexDenseGeneric

    basicAddressPopCount = \form@(FormatColumnStrided size _) (Range loA hiA)->
      let newForm = (FormatColumnContiguous size)
        in
          basicAddressPopCount  newForm
            (Range (basicToDenseAddress newForm $ basicToDenseIndex form loA)
                 (basicToDenseAddress newForm $ basicToDenseIndex form hiA) )

    basicAddressAsInt = \ _ (Address a) -> a

    {-# INLINE basicAddressRange #-}
    {-# INLINE indexToAddress #-}
    {-# INLINE addressToIndex #-}
    {-# INLINE basicNextAddress #-}
    {-# INLINE basicNextIndex #-}
    {-# INLINE basicAddressPopCount #-}

----------------------
----------------------
-----
-----
----------------------
----------------------



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
                                  put $! (val * accum) ;
                                  return accum;




-----
-----
-----

instance DenseLayout (Format Direct 'Contiguous ('S 'Z) rep)  ('S 'Z)  where


    --maxDenseAddress = \ (FormatDirectContiguous ix) -> Address (ix -1)


    {-#INLINE basicToDenseAddress #-}
    basicToDenseAddress   = \ (FormatDirectContiguous _) (j :* _ ) -> Address j

    --basicNextIndex=  undefined -- \ _ x ->  Just $! x + 1
    --note its unchecked!
    {-# INLINE basicToDenseIndex #-}
    basicToDenseIndex =  \ (FormatDirectContiguous _) (Address ix)  -> (ix ) :* Nil

    {-# INLINE basicNextDenseAddress #-}
    basicNextDenseAddress = \ _ addr -> addr + 1





instance DenseLayout (Format Direct 'Strided ('S 'Z) rep)  ('S 'Z)  where




    {-#INLINE basicToDenseAddress #-}
    basicToDenseAddress   = \ (FormatDirectStrided _ strid) (j :* Nil )->  Address (strid * j)

    {-# INLINE basicNextDenseAddress #-}
    basicNextDenseAddress = \ (FormatDirectStrided _ strid) addr ->  addr + Address strid

    {-# INLINE basicNextDenseIndex #-}
    basicNextDenseIndex =  \ form  (i:* Nil ) ->  (\ix -> (ix,basicToDenseAddress form ix)) $! (i + 1 :* Nil )


    {-# INLINE basicToDenseIndex #-}
    basicToDenseIndex = \ (FormatDirectStrided _ stride) (Address ix)  -> (ix `div` stride ) :* Nil


-----
-----
-----



-- strideRow :: Shape rank Int,
instance   (Applicative (Shape rank),F.Foldable (Shape rank), Traversable (Shape rank))  =>
    DenseLayout (Format Row  'Contiguous rank rep) rank where

{-
TODO  AUDIT

-}
    {-# INLINE basicToDenseAddress #-}
    --indexToAddress = \rs tup -> let !strider =takePrefix $! S.scanr (*) 1 (boundsFormRow rs)
    basicToDenseAddress = \rs tup ->
          let !strider =  computeStrideShape traverse (boundsFormRow rs)
                  in Address $! S.foldl'  (+) 0 $! map2 (*) strider tup

    {-# INLINE basicNextDenseAddress #-}
    basicNextDenseAddress = \_ addr -> addr + 1

    {-# INLINE basicToDenseIndex #-}
    basicToDenseIndex  =   \ rs (Address ix) ->
        let !striderShape  = computeStrideShape traverse (boundsFormRow rs)

            in
               flip evalState ix $
                  flip (S.backwards traverse)  striderShape $
                  -- want to start from largest stride (which is on the right)
                      \ currentStride ->
                             do remainderIx <- get ;
                                let (!qt,!rm)= quotRem remainderIx currentStride
                                put  $! rm
                                return  qt;




-----
-----

-- strideRow :: Shape rank Int,
instance   (Applicative (Shape rank),F.Foldable (Shape rank), Traversable (Shape rank))
  => DenseLayout (Format Row  'InnerContiguous rank rep) rank  where


    {-# INLINE basicToDenseAddress #-}
    basicToDenseAddress = \rs tup ->
                       Address $! S.foldl'  (+) 0 $!
                         map2 (*) (strideFormRowInnerContig rs ) tup

    {-# INLINE basicNextDenseIndex #-}
    basicNextDenseIndex = \ form@(FormatRowInnerContiguous shape _) ix ->
        --S.map snd $!
      (\index -> (index,basicToDenseAddress form  index)) $!
        flip evalState 1 $
           for   ((,) <$> ix <*> shape) $
              \(ixv ,shpv   )->
                  do  carry <-get
                      let (newCarry,modVal)=divMod (carry + ixv) shpv
                      put $! newCarry
                      return modVal


    {-# INLINE basicToDenseIndex #-}
    basicToDenseIndex  =   \ rs (Address ix) ->   flip evalState ix $
                          flip ( S.backwards traverse)  (strideFormRowInnerContig rs ) $
                              \ currentStride ->
                                     do remainderIx <- get ;
                                        let (!qt,!rm)= quotRem remainderIx currentStride
                                        put $! rm
                                        return  qt;



---
---
-- strideRow :: Shape rank Int,

instance  (Applicative (Shape rank),F.Foldable (Shape rank), Traversable (Shape rank))
  => DenseLayout (Format Row 'Strided rank rep) rank  where



    {-# INLINE basicToDenseAddress #-}
    basicToDenseAddress = \rs tup ->   Address $!
          S.foldl'  (+) 0 $! map2 (*) (strideFormRowStrided rs ) tup

    {-# INLINE basicNextDenseIndex #-}
    basicNextDenseIndex = \ form@(FormatRowStrided shape _) ix ->
      (\index -> (index,basicToDenseAddress form index)) $!
        flip evalState 1 $
           for  ((,) <$> ix <*> shape) $
              \(ixv ,shpv   )->
                  do  carry <-get
                      let (newCarry,modVal)=divMod (carry + ixv) shpv
                      put $! newCarry
                      return modVal


    {-# INLINE basicToDenseIndex #-}
    basicToDenseIndex  =   \ rs (Address ix) ->   flip evalState ix $
                          flip (S.backwards traverse ) (strideFormRowStrided rs ) $
                              \ currentStride ->
                                     do remainderIx <- get ;
                                        let (!qt,!rm)= quotRem remainderIx currentStride
                                        put $!  rm
                                        return  qt;




-----
-----
-----


 -- strideRow :: Shape rank Int,
instance  (Applicative (Shape rank),F.Foldable (Shape rank), Traversable (Shape rank))
  => DenseLayout (Format Column  'Contiguous rank rep)  rank where



    {-# INLINE basicToDenseAddress #-}
    basicToDenseAddress = \rs tup ->
          let !strider = computeStrideShape  (S.backwards traverse) (boundsColumnContig rs)
                                in Address $! S.foldl'  (+) 0 $! map2 (*) strider tup

    {-# INLINE basicNextDenseAddress #-}
    basicNextDenseAddress = \_ addr -> addr + 1

    {-# INLINE basicToDenseIndex #-}
    basicToDenseIndex  =   \ rs (Address ix) ->
            let !striderShape  =  computeStrideShape  (S.backwards traverse) (boundsColumnContig rs)
                in
                   flip evalState ix $
                        for  striderShape $
                              \ currentStride ->
                                     do remainderIx <- get ;
                                        let (!qt,!rm)= quotRem remainderIx currentStride
                                        put $!  rm
                                        return  qt;





 -- strideRow :: Shape rank Int,
instance  (Applicative (Shape rank),F.Foldable (Shape rank), Traversable (Shape rank))
  => DenseLayout (Format Column  'InnerContiguous rank rep) rank  where


    {-# INLINE basicToDenseAddress #-}
    basicToDenseAddress    =   \ form tup -> let !strider =   strideFormColumnInnerContig form
                                in Address $! foldl' (+) 0  $! map2 (*) strider tup
    {-# INLINE basicNextDenseIndex #-}
    basicNextDenseIndex = \ form@(FormatColumnInnerContiguous shape _) ix ->
        --S.map snd $!
      (\index -> (index,basicToDenseAddress form index)) $!
        flip evalState 1 $
           flip (S.backwards traverse)  ((,) <$> ix <*> shape) $
              \(ixv ,shpv   )->
                  do  carry <-get
                      let (newCarry,modVal)=divMod (carry + ixv) shpv
                      put $! newCarry
                      return modVal


    {-# INLINE basicToDenseIndex #-}
    basicToDenseIndex  =   \ rs (Address ix) ->   flip evalState ix $
                          flip S.traverse  (strideFormColumnInnerContig rs ) $
                              \ currentStride ->
                                     do remainderIx <- get ;
                                        let (!qt,!rm)= quotRem remainderIx currentStride
                                        put $! rm
                                        return  qt;




instance   (Applicative (Shape rank),F.Foldable (Shape rank), Traversable (Shape rank))
  => DenseLayout (Format Column  'Strided rank rep) rank where

    {-# INLINE basicToDenseAddress #-}
    basicToDenseAddress    =   \ form tup -> let !strider =   strideFormColumnStrided form
                                in Address $! foldl' (+) 0  $! map2 (*) strider tup

    {-# INLINE basicNextDenseIndex #-}
    basicNextDenseIndex = \ form@(FormatColumnStrided shape _) ix ->
        --S.map snd $!
      (\index -> (index,basicToDenseAddress form index)) $!
        flip evalState 1 $
           flip (S.backwards traverse)  ((,) <$> ix <*> shape) $
              \(ixv ,shpv   )->
                  do  carry <-get
                      let (newCarry,modVal)=divMod (carry + ixv) shpv
                      put $! newCarry
                      return modVal


    {-# INLINE basicToDenseIndex #-}
    basicToDenseIndex  =   \ rs (Address ix) ->   flip evalState ix $
                          flip S.traverse  (strideFormColumnStrided rs ) $
                              \ currentStride ->
                                     do remainderIx <- get ;
                                        let (!qt,!rm)= quotRem remainderIx currentStride
                                        put $! rm
                                        return  qt;






{-
*Numerical.Array.Layout> indexToAddress (FormColumn (2 :* 3 :* 7 :* Nil)) (0:* 2 :* 2 :* Nil)
Address 16
*Numerical.Array.Layout> indexToAddress (FormColumn (2 :* 3 :* 7 :* Nil)) (1:* 0 :* 0 :* Nil)
Address 1
*Numerical.Array.Layout> indexToAddress (FormColumn (2 :* 3 :* 7 :* Nil)) (0:* 0 :* 0 :* Nil)
Address 0
*Numerical.Array.Layout> indexToAddress (FormColumn (2 :* 3 :* 7 :* Nil)) (0:* 1 :* 0 :* Nil)
Address 2
*Numerical.Array.Layout> indexToAddress (FormColumn (2 :* 3 :* 7 :* Nil)) (0:* 0 :* 1 :* Nil)
-}


