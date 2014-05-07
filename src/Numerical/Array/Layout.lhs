\begin{code}



{-# LANGUAGE PolyKinds   #-}
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

module Numerical.Array.Layout(
  Locality(..)
  ,Format(..)
  ,Row
  ,Column
  ,Direct
  ,Layout(..)
  ,Address(..)
  ,UniformAddressInterval(..)
  ,Ordering) where



import Numerical.Nat
import Control.Applicative
import Numerical.Array.Address
import Numerical.Array.Locality
import Numerical.Array.Shape as S
import Data.Traversable (Traversable)

import Control.NumericalMonad.State.Strict

import qualified Data.Foldable as F

import Prelude hiding (foldr,foldl,map,scanl,scanr,scanl1,scanr1)

{-|  A major design goal with the Layout module is to
make it easy to define new dense array layouts



-}


--data PrimLay a
--data StaticLay a
--data Lay a


data Direct

data Row

data Column


{-
NB: may need to add some specialization for low rank indexing,
theres 4 choices:
a) INLINE EVERYTHING
b) rewrite rules that take low rank indexing code into specialized versions thereof
c) wait till ghc 7.8.2 to resolve https://ghc.haskell.org/trac/ghc/ticket/8848
    and use SPECIALIZE
d) benchmark and then decide

for now I choose (e), defer benchmarking till everything works :)


a related concern is the interplay of inlining and specialization
https://ghc.haskell.org/trac/ghc/ticket/5928

-}


{-
note also that this is in practice a *dense* only
layout module, though a derived api can interpret those
formats sparsely



-}

{-
TODO : move a bunch of the projection and transformation
logic from the Array classes to the analogous Layout classes.
All of the operations are oblivious to underlying buffer type

-}

-- either we need to break ties, or the ties have been broken
majorCompareLeftToRight :: Ordering -> Ordering -> Ordering
majorCompareLeftToRight EQ new = new
majorCompareLeftToRight a _ = a


majorCompareRightToLeft :: Ordering -> Ordering -> Ordering
majorCompareRightToLeft new EQ = new
majorCompareRightToLeft _ b = b

data family Format lay (contiguity:: Locality)  (rank :: Nat)


{-
LAYOUT as currently defined is actually dense layout

need to add a general sparse/dense layout format distinction


FOR NOW, punting on sparse for alpha,
will need to make breaking changes to LAYOUT later for sparse



ok, the crucial distinction between sparse and dense is that
for Dense: Index -> Address when inbounds will always succeed

but for Sparse : Index -> Address can fail even when its in bounds
-}


class Layout form rank | form -> rank where

    --type Tranposed form


    --transposedLayout ::  (form ~ Tranposed transform,transform~Tranposed form)=> form  -> transform

    -- not happy with this name, will change later FIXME TODO
    --basicFormShape :: form -> Shape rank Int

    --basicCompareIndex :: p form-> Shape rank Int ->Shape rank Int -> Ordering


class Layout form rank => SparseLayout  form  (rank :: Nat) | form -> rank  where



  lookupIndex :: form -> Index rank -> Maybe Address


  --note that unlike the Layout method  basicToAddress,





--class Layout form rank => DenseLayout  form  (rank :: Nat) | form -> rank  where
  {-
  empty class instances for all the dense Layouts
  -}


class DenseLayout form  (rank :: Nat) | form -> rank  where

    type Tranposed form

    -- not happy with this name, will change later FIXME TODO
    basicFormShape :: form -> Shape rank Int

    transposedLayout ::  (form ~ Tranposed transform,transform~Tranposed form)=> form  -> transform
    --shapeOf

    basicToAddress :: form  -> Shape rank Int ->   Address


    basicToIndex :: form -> Address -> Shape rank Int

    --unchecked
    --nextAddress --- not sure if this should even exist for contiguous ones..
    -- not sure if this is the right model for the valid ops
    --validAddress::Form   lay contiguity rank -> Int -> Either String (Shape rank Int)
    --validIndex ::Form   lay contiguity rank -> Shape rank Int -> Either String Int

    -- IMPORTANT NOTE, the NextAddress defined via Next Index seems
    -- that it will only be invoked on strided/discontiguous dense formats
    -- this
    basicNextAddress :: form  -> Address ->  Address
    basicNextAddress =  \form shp ->  basicToAddress form $
      (basicNextIndex form  $ basicToIndex form  shp )
    {-# INLINE basicNextAddress #-}

    basicNextIndex :: form  -> Shape rank Int ->Shape rank Int
    basicNextIndex  = \form shp ->  basicToIndex form  $
       (basicNextAddress form  $ basicToAddress form  shp )
    {-# INLINE  basicNextIndex #-}


    basicCompareIndex :: p form-> Shape rank Int ->Shape rank Int -> Ordering



    -- one of basicNextAddress and basicNextIndex must always be implemented
    {-# MINIMAL basicFormShape,basicCompareIndex, transposedLayout, basicToIndex, basicToAddress, (basicNextIndex | basicNextAddress)  #-}

-----
-----
-----

data instance Format  Direct Contiguous (S Z) =
            FormatDirectContiguous { logicalShapeDirectContiguous :: {-#UNPACK#-} !Int }

instance DenseLayout (Format Direct Contiguous (S Z))  (S Z)  where

    type Tranposed (Format Direct Contiguous (S Z)) = (Format Direct Contiguous (S Z))

    {-# INLINE basicFormShape#-}
    basicFormShape = \ x -> (logicalShapeDirectContiguous x) :* Nil

    transposedLayout = id

    {-#INLINE basicToAddress#-}
    basicToAddress   = \ (FormatDirectContiguous _) (j :* _ ) -> Address j

    --basicNextIndex=  undefined -- \ _ x ->  Just $! x + 1
    --note its unchecked!
    {-# INLINE basicToIndex#-}
    basicToIndex =  \ (FormatDirectContiguous _) (Address ix)  -> (ix ) :* Nil

    {-# INLINE basicNextAddress #-}
    basicNextAddress = \ _ addr -> addr + 1

    {-# INLINE basicCompareIndex #-}
    basicCompareIndex = \ _  (l:* _) (r:* _) -> compare l r



data instance Format  Direct Strided (S Z) =
        FormatDirectStrided { logicalShapeDirectStrided :: {-#UNPACK#-}!Int
                    , logicalStrideDirectStrided:: {-#UNPACK#-}!Int}

instance DenseLayout (Format Direct Strided (S Z))  (S Z)  where

    type Tranposed (Format Direct Strided (S Z)) = (Format Direct Strided (S Z))

    {-# INLINE basicFormShape #-}
    basicFormShape = \x -> (logicalShapeDirectStrided x) :* Nil

    transposedLayout = id

    {-#INLINE basicToAddress#-}
    basicToAddress   = \ (FormatDirectStrided _ strid) (j :* Nil )->  Address (strid * j)

    {-# INLINE basicNextAddress #-}
    basicNextAddress = \ (FormatDirectStrided _ strid) addr ->  addr + Address strid

    {-# INLINE basicNextIndex#-}
    basicNextIndex =  \ _  (i:* Nil ) ->  (i + 1 :* Nil )


    {-# INLINE basicToIndex#-}
    basicToIndex = \ (FormatDirectStrided _ stride) (Address ix)  -> (ix `div` stride ) :* Nil

    {-# INLINE basicCompareIndex #-}
    basicCompareIndex = \ _  (l:* _) (r:* _) -> compare l r

-----
-----
-----



data instance  Format  Row  Contiguous rank  = FormatRowContiguous {
    boundsFormRow :: !(Shape rank Int)}

-- strideRow :: Shape rank Int,
instance   (Applicative (Shape rank),F.Foldable (Shape rank), Traversable (Shape rank))
    => DenseLayout (Format Row  Contiguous rank) rank where

    type Tranposed (Format Row  Contiguous rank) = (Format Column Contiguous rank)

    transposedLayout = \(FormatRowContiguous shp) -> FormatColumnContiguous $ reverseShape shp

    {-# INLINE basicFormShape #-}
    basicFormShape = \x -> boundsFormRow x

    {-# INLINE basicToAddress #-}
    --basicToAddress = \rs tup -> let !strider =takePrefix $! S.scanr (*) 1 (boundsFormRow rs)
    basicToAddress = \rs tup -> let !strider = flip evalState 1 $
                                      flip (S.backwards traverse) (boundsFormRow rs) $
                                        \ val ->
                                               do accum <- get ;
                                                  put accum ; -- NOT val * accum
                                                  return (val * accum);
                                in Address $! S.foldl'  (+) 0 $! map2 (*) strider tup
    {-# INLINE basicNextAddress#-}
    basicNextAddress = \_ addr -> addr + 1

    {-# INLINE basicToIndex #-}
    basicToIndex  =   \ rs (Address ix) -> case boundsFormRow rs of
          Nil -> Nil
          (_:*_)->
            --let !striderShape  =takePrefix $! S.scanr (*) 1 (boundsFormRow rs)
            -- FIXME
            let !striderShape  =  flip evalState 1 $
                                      -- FIXME not sure if this should be reverse
                                    flip (S.backwards traverse) (boundsFormRow rs) $
                                        \ val ->
                                             do accum <- get ;
                                                put (val * accum) ;
                                                return (val * accum);
                in
                   flip evalState ix $
                          flip S.traverse  striderShape $
                              \ currentStride ->
                                     do remainderIx <- get ;
                                        let (!qt,!rm)= quotRem remainderIx currentStride
                                        put rm
                                        return  qt;


    {-# INLINE basicCompareIndex #-}
    basicCompareIndex = \ _  ls rs -> foldl majorCompareLeftToRight EQ  $ S.map2 compare ls rs


-----
-----
data instance  Format  Row  InnerContiguous rank  =
        FormatRowInnerContiguous {boundsFormRowInnerContig :: !(Shape rank Int)
        , strideFormRowInnerContig:: !(Shape rank Int)}

-- strideRow :: Shape rank Int,
instance   (Applicative (Shape rank),F.Foldable (Shape rank), Traversable (Shape rank))
  => DenseLayout (Format Row  InnerContiguous rank) rank  where

    type Tranposed (Format Row  InnerContiguous rank) = Format Column  InnerContiguous rank

    {-# INLINE basicFormShape  #-}
    basicFormShape = \x -> boundsFormRowInnerContig x

    transposedLayout = \(FormatRowInnerContiguous shp stride) ->
        FormatColumnInnerContiguous  (reverseShape shp)  (reverseShape stride)

    {-# INLINE basicToAddress #-}
    basicToAddress = \rs tup ->
                       Address $! S.foldl'  (+) 0 $!
                         map2 (*) (strideFormRowInnerContig rs ) tup

    {-# INLINE basicNextIndex #-}
    basicNextIndex = \ (FormatRowInnerContiguous shape _) ix ->
        --S.map snd $!
        flip evalState 1 $
           flip traverse  ((,) <$> ix <*> shape) $
              \(ixv ,shpv   )->
                  do  carry <-get
                      let (newCarry,modVal)=divMod (carry + ixv) shpv
                      put newCarry
                      return modVal


    {-# INLINE basicToIndex #-}
    basicToIndex  =   \ rs (Address ix) ->   flip evalState ix $
                          flip S.traverse  (strideFormRowInnerContig rs ) $
                              \ currentStride ->
                                     do remainderIx <- get ;
                                        let (!qt,!rm)= quotRem remainderIx currentStride
                                        put rm
                                        return  qt;


    {-# INLINE basicCompareIndex #-}
    basicCompareIndex = \ _  ls rs ->
      foldl majorCompareLeftToRight EQ  $ S.map2 compare ls rs

---
---
data instance  Format  Row  Strided rank  =
        FormatRowStrided {boundsFormRowStrided:: !(Shape rank Int)
            , strideFormRowStrided:: !(Shape rank Int)}
-- strideRow :: Shape rank Int,

instance  (Applicative (Shape rank),F.Foldable (Shape rank), Traversable (Shape rank))
  => DenseLayout (Format Row  Strided rank) rank  where

    type Tranposed (Format Row  Strided rank) = (Format Column  Strided rank)

    {-# INLINE basicFormShape  #-}
    basicFormShape = \x -> boundsFormRowStrided x

    transposedLayout = \(FormatRowStrided shp stride) ->
        FormatColumnStrided  (reverseShape shp)  (reverseShape stride)

    {-# INLINE basicToAddress #-}
    basicToAddress = \rs tup ->   Address $!
          S.foldl'  (+) 0 $! map2 (*) (strideFormRowStrided rs ) tup

    {-# INLINE basicNextIndex #-}
    basicNextIndex = \ (FormatRowStrided shape _) ix ->
        --S.map snd $!
        flip evalState 1 $
           flip traverse  ((,) <$> ix <*> shape) $
              \(ixv ,shpv   )->
                  do  carry <-get
                      let (newCarry,modVal)=divMod (carry + ixv) shpv
                      put newCarry
                      return modVal


    {-# INLINE basicToIndex #-}
    basicToIndex  =   \ rs (Address ix) ->   flip evalState ix $
                          flip S.traverse  (strideFormRowStrided rs ) $
                              \ currentStride ->
                                     do remainderIx <- get ;
                                        let (!qt,!rm)= quotRem remainderIx currentStride
                                        put rm
                                        return  qt;




    {-# INLINE basicCompareIndex #-}
    basicCompareIndex = \ _  ls rs ->
        foldl majorCompareLeftToRight EQ  $ S.map2 compare ls rs

-----
-----
-----

data instance  Format  Column Contiguous rank  = FormatColumnContiguous {
  boundsColumnContig :: !(Shape rank Int)}

 -- strideRow :: Shape rank Int,
instance  (Applicative (Shape rank),F.Foldable (Shape rank), Traversable (Shape rank))
  => DenseLayout (Format Column  Contiguous rank )  rank where

    type Tranposed (Format Column  Contiguous rank ) = (Format Row Contiguous rank )

    {-# INLINE basicFormShape  #-}
    basicFormShape = \x -> boundsColumnContig x

    transposedLayout = \(FormatColumnContiguous shp)-> FormatRowContiguous $ reverseShape shp

    {-# INLINE basicToAddress #-}

    --basicToAddress = \rs tup -> let !strider =takePrefix $! S.scanr (*) 1 (boundsFormRow rs)
    basicToAddress = \rs tup -> let !strider = flip evalState 1 $
                                      flip S.traverse (boundsColumnContig rs) $
                                        \ val ->
                                               do accum <- get ;
                                                  put accum ; -- NOT val * accum
                                                  return (val * accum);
                                in Address $! S.foldl'  (+) 0 $! map2 (*) strider tup
    {-# INLINE basicNextAddress#-}
    basicNextAddress = \_ addr -> addr + 1

    {-# INLINE basicToIndex #-}
    basicToIndex  =   \ rs (Address ix) -> case boundsColumnContig rs of
          Nil -> Nil
          (_:*_)->
            --let !striderShape  =takePrefix $! S.scanr (*) 1 (boundsFormRow rs)
            -- FIXME
            let !striderShape  =  flip evalState 1 $
                                      -- FIXME not sure if this should be reverse
                                    flip S.traverse (boundsColumnContig rs) $
                                        \ val ->
                                             do accum <- get ;
                                                put (val * accum) ;
                                                return (val * accum);
                in
                   flip evalState ix $
                          flip (S.backwards traverse)  striderShape $
                              \ currentStride ->
                                     do remainderIx <- get ;
                                        let (!qt,!rm)= quotRem remainderIx currentStride
                                        put rm
                                        return  qt;

    {-# INLINE basicCompareIndex #-}
    basicCompareIndex = \ _  ls rs -> foldr majorCompareRightToLeft EQ  $ S.map2 compare ls rs


data instance  Format Column InnerContiguous rank  = FormatColumnInnerContiguous {
      boundsColumnInnerContig :: !(Shape rank Int),
      strideFormColumnInnerContig:: !(Shape rank Int)}

 -- strideRow :: Shape rank Int,
instance  (Applicative (Shape rank),F.Foldable (Shape rank), Traversable (Shape rank))
  => DenseLayout (Format Column  InnerContiguous rank) rank  where

    type Tranposed (Format Column  InnerContiguous rank) = (Format Row  InnerContiguous rank)

    {-# INLINE basicFormShape  #-}
    basicFormShape = \x -> boundsColumnInnerContig x

    transposedLayout = \(FormatColumnInnerContiguous shp stride)->
         FormatRowInnerContiguous (reverseShape shp) (reverseShape stride)

    {-# INLINE basicToAddress #-}
    basicToAddress    =   \ form tup -> let !strider =   strideFormColumnInnerContig form
                                in Address $! foldl' (+) 0  $! map2 (*) strider tup
    {-# INLINE basicNextIndex #-}
    basicNextIndex = \ (FormatColumnInnerContiguous shape _) ix ->
        --S.map snd $!
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

    {-# INLINE basicCompareIndex #-}
    basicCompareIndex = \ _  ls rs -> foldr majorCompareRightToLeft EQ  $ S.map2 compare ls rs


data instance  Format Column Strided rank  = FormatColumnStrided {
    boundsColumnStrided :: !(Shape rank Int)
    , strideFormColumnStrided:: !(Shape rank Int)}
 -- strideRow :: Shape rank Int,


instance   (Applicative (Shape rank),F.Foldable (Shape rank), Traversable (Shape rank))
  => DenseLayout (Format Column  Strided rank) rank where

    type Tranposed (Format Column  Strided rank) = (Format Row  Strided rank)

    {-# INLINE basicFormShape  #-}
    basicFormShape = \x -> boundsColumnStrided x

    transposedLayout = \(FormatColumnStrided shp stride)->
         FormatRowStrided (reverseShape shp) (reverseShape stride)

    {-# INLINE basicToAddress #-}
    basicToAddress    =   \ form tup -> let !strider =   strideFormColumnStrided form
                                in Address $! foldl' (+) 0  $! map2 (*) strider tup

    {-# INLINE basicNextIndex #-}
    basicNextIndex = \ (FormatColumnStrided shape _) ix ->
        --S.map snd $!
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


    {-# INLINE basicCompareIndex #-}
    basicCompareIndex = \ _  ls rs -> foldr majorCompareRightToLeft EQ $ S.map2 compare ls rs



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


--data Elem ls el  where
--    Point :: Elem '[] el
--    (:#) :: a -> Elem ls el -> Elem (a ': ls) el


{-
    One important invariant about all layouts at all ranks is that for
    any given ints x < y, that the array index for inr

     toIndex shapedLayout (pure x :: Shape rank Int) is strictly less than
     toIndex shapedLayout (pure y :: Shape rank Int).

     more generally

     for rank k tuples,
      xi = x_1 :* ... :* x_k *: Nil  and
      yj = y_1 :* ... :* x_k *: Nil
      such that forall \ell, x_\ell  < y_\ell
    we have that
       toIndex shapedLayout xi <  toIndex  shapedLayout yj


this actually relates to the notion of partial ordering over vectors in convex
geometry!


so roughly: we have layouts that are dense
we have layouts that can be used as tiles (and are dense)

and we have layouts which can can't be tiled, but can have elements which are tiled

So we have

PrimitiveLayouts

Static Layouts

General Layouts (which are a Top level layout over a static layout)

the Layout class tries to abstract over all three cases
(NB: this only makes sense when the "rank" for the inner
and outer layouts have the same rank!)

-}


{- Sized is used as a sort of hack to make it easy to express
   the staticly sized layouts. NB, one trade off is that its only
   possible to express  "cube" shaped blocks, but on the other
   hand blocking sizes are expressible for every single rank!
-}
--data Sized :: * -> * where
    --(:@) :: Nat -> a -> Sized a


{-

per se I don't need the StaticLay, PrimLay, Lay constructors, BUT
I really do like how it makes things a teeny bit simpler.. though I may remove them
-}



--class SimpleDenseLayout lay (rank :: Nat) where
--  type SimpleDenseTranpose lay
--  toIndexSimpleDense :: Shaped rank Int lay -> Shape rank Int -> Int


--class PrimLayout lay (rank :: Nat) where
--    type TranposedPrim lay
--    toIndexPrim :: Shaped rank Int (PrimLay lay) -> Shape rank Int -> Int
--    fromIndexPrim :: Shaped rank Int (PrimLay lay) -> Int -> Shape rank Int


{-
for now we will not deal with nested formats, but this will
be a breaking change i plan for later
-}

{-
what is the law for the Layout class?
forall valid formms
toIndex sd  (fromIndex sd ix)==ix
fromIndex sd (toIndex sd shp)==shp
-}

{-
if   tup1 is strictly less than tup2 (pointwise),
  then any lawful Layout will asign tup1 an index strictly less than that
  asigned to tup2

  transposedLayout . transposedLayout == id



i treat coordinates as being in x:* y :* z :* Nil, which is Fortran style idexing

in row major we'd have for x:* y :* Nil that X is the inner dimension, and y the outter,
by contrast, in column major, y would be the inner most, and x the outter most.




-}


{- In some respects, the Layout type class is a multidimensional
analogue of the Enum type class in Haskell Prelude,
for Dense / Dense Structured matrix formats
but
    a) requires a witness value, the "Form"
    b) needs to handle multivariate structures
    c) has to deal with structure matrices, like triangular, symmetric, etc
    e) I think every layout should have pure 0 be a valid index, at least for "Dense"
    arrays
    f) transposedLayout . transposedLayout == id
    g)

  Form needs to carry the shape / extent of the matrix

-}
{-

-}

--data View = Origin | Slice
{-
i'm really really hoping to not need a View parameter,
but the nature of the addressing logic needs to change when its a slice
vs a deep copy (for certain classes of arrays that I wish to support very easily)

I will be likely adding this the moment benchmarks validate the distinction

on the
-}
\end{code}
