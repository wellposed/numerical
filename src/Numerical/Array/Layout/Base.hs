{- |  Comments for this modules


-}

-- {-# LANGUAGE PolyKinds   #-}
-- {-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-#  LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE CPP #-}

{-# LANGUAGE StandaloneDeriving #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE UndecidableInstances #-}

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 707
 {-# LANGUAGE AutoDeriveTypeable #-}
#endif


module Numerical.Array.Layout.Base(
  Layout(..)
  ,DenseLayout(..)
  ,RectilinearLayout(..)
  ,LayoutAddress
  ,Transposed
  ,FormatStorageRep
  ,RectOrientationForm
  ,RectDownRankForm
  ,InnerContigForm
  ,Format
  ,TaggedShape(..)
  ,GDSlice(..) --- right? right?
  ,SMajorOrientation(..)
  ,MajorOrientation(..)
  ,majorCompareRightToLeft
  ,majorCompareLeftToRight
  ,shapeCompareRightToLeft
  ,shapeCompareLeftToRight
  -- * All the various helper types
  ,module Numerical.Array.Storage
  ,module Numerical.Array.Locality
  ,module Numerical.Array.Shape
  ,module Numerical.Array.Range
  ,module Numerical.Array.Address
) where


import Data.Data

import Numerical.Nat
import Numerical.Array.Address
import Numerical.Array.Locality
import Numerical.Array.Shape
import Numerical.Array.Storage
import Numerical.Array.Range

--import Data.Typeable
import Control.Applicative as A


--import Control.NumericalMonad.State.Strict
import qualified Data.Foldable as F

import Prelude hiding (foldr,foldl,map,scanl,scanr,scanl1,scanr1)


{-
NB: may need to add some specialization for low rank indexing,
theres 4 choices:
a) INLINE EVERYTHING
b) rewrite rules that take low rank indexing code into specialized versions thereof
c) wait till ghc 7.8.2 to resolve https://ghc.haskell.org/trac/ghc/ticket/8848
    and use SPECIALIZE
d) benchmark and then decide

for now I choose (a), and defer benchmarking variations till everything works :)


a related concern is the interplay of inlining and specialization
https://ghc.haskell.org/trac/ghc/ticket/5928

-}





-- either we need to break ties, or the ties have been broken
majorCompareLeftToRight :: Ordering -> Ordering -> Ordering
majorCompareLeftToRight EQ new = new
majorCompareLeftToRight a _ = a


majorCompareRightToLeft :: Ordering -> Ordering -> Ordering
majorCompareRightToLeft new EQ = new
majorCompareRightToLeft _ b = b

{-# INLINE shapeCompareLeftToRight #-}
shapeCompareLeftToRight :: (F.Foldable (Shape r),A.Applicative (Shape r), Ord a)
    => Shape r a -> Shape r a -> Ordering
shapeCompareLeftToRight =   \  ls rs -> foldl majorCompareLeftToRight EQ  $ map2 compare ls rs

{-# INLINE shapeCompareRightToLeft #-}
shapeCompareRightToLeft :: ((F.Foldable (Shape r)),A.Applicative (Shape r), Ord a)
   => Shape r a -> Shape r a -> Ordering
shapeCompareRightToLeft =   \  ls rs -> foldl majorCompareRightToLeft EQ  $ map2 compare ls rs




-- | this is kinda a hack
newtype TaggedShape (form :: *) (rank::Nat) = TaggedShape {unTagShape:: Shape rank Int }
instance Eq (Shape rank Int)=> Eq (TaggedShape f rank) where
  (==) l r =  (==) (unTagShape l) (unTagShape r )

instance Show (Shape rank Int) => Show (TaggedShape f rank) where
  show (TaggedShape ix) =  "TaggedShape (" ++ show ix ++ " )"

instance forall form  rank . (Eq (Shape rank Int),Layout form rank)
  => Ord (TaggedShape form rank) where
  compare left right = basicCompareIndex (Proxy:: Proxy form ) (unTagShape left) (unTagShape right)


-- | Generalized Dense Slice Projection notation,
-- not sure if it should be defined in this module or elsewhere
data GDSlice (from :: Nat) (to :: Nat) :: *  where
  GDNil :: GDSlice Z Z
  GDPick :: Int -> GDSlice from to -> GDSlice (S from) to
  GDRange :: (Int,Int,Int)-> GDSlice from to -> GDSlice (S from) (S to)
  GDAll :: GDSlice from to -> GDSlice (S from) (S to)





data family Format  lay (contiguity:: Locality)  (rank :: Nat) rep

deriving instance Typeable Format

type family FormatStorageRep ( a:: * ) :: *

type instance FormatStorageRep (Format lay ctg rnk rep)= rep

type family  Transposed form

type family  LayoutAddress (form :: *)


-- |
class Layout form  (rank :: Nat) | form -> rank  where

    -- not happy with this name, will change later FIXME TODO
    -- | 'basicFormLogicalShape' gives the extent of the format
    basicFormLogicalShape :: form -> Shape rank Int

    -- | 'transposedLayout' transposes the format data type
    transposedLayout :: (form ~ Transposed transform,transform~Transposed form)=> form  -> transform

    -- | 'basicCompareIndex' lets you compare where two (presumably inbounds)
    -- 'Index' values are in a formats ordering. The logical 'Shape' of the array
    -- is not needed
    basicCompareIndex :: p form-> Shape rank Int ->Shape rank Int -> Ordering

    -- FIX ME TODO: need to have an extra wide int type to do logical address correctly
    -- this is because spare arrays can have much larger range of dimensions
    -- than are representable in physical memory
    -- but dont want to pay for the extended dynamic range when I dont need it
    --  -- | Given a 'Format' with a known Shape, aka a 'TaggedShape', we can
    -- always relate any in-bounds index to the corresponding address a
    -- dense 'Contiguous' analogue of the format will have.
    -- this is also used to provide efficient sorting for correctly constructing
    -- arrays efficiently that breaks the @n*log(n)@ barrier of comparison based
    -- methods.
    --basicLogicalAddress :: TaggedShape form rank -> Shape rank Int -> Int

    -- | the (possibly empty) min and max of the valid addresses for a given format
    rangedFormatAddress ::  (address ~ LayoutAddress form)=> form -> Maybe (Range address)
    -- FIX ME! this name is crap, i dont like it

    basicToAddress :: (address ~ LayoutAddress form)=>
        form  -> Shape rank Int -> Maybe  address

    basicToIndex ::(address ~ LayoutAddress form)=>
        form -> address -> Shape rank Int

    basicNextAddress :: (address ~ LayoutAddress form)=>
        form  -> address -> Maybe  address

    basicNextIndex :: (address ~ LayoutAddress form)=>
          form  -> Shape rank Int-> Maybe address  -> Maybe ( Shape rank Int, address)


    basicAddressPopCount :: (address ~ LayoutAddress form)=>
        form -> Range address -> Int

    {-# MINIMAL basicToAddress, basicToIndex, basicNextAddress,basicNextIndex
          ,rangedFormatAddress,basicFormLogicalShape,basicCompareIndex
          , transposedLayout, basicAddressPopCount #-}


{-
these names aren't ideal, but lets punt on bikeshedding till theres >= 2 serious
users
-}
data MajorOrientation = Rowed | Columned | BlockedColumn | BlockedRow
  deriving(Data,Typeable)

data SMajorOrientation (o :: MajorOrientation) where
    SRowed :: SMajorOrientation Rowed
    SColumned :: SMajorOrientation Columned
    SBlockedRow :: SMajorOrientation BlockedRow
    SBlockedColumn :: SMajorOrientation BlockedColumn



type family RectOrientationForm form :: MajorOrientation
type family RectDownRankForm   form :: *
type family InnerContigForm form :: *

{- | 'RectilinearLayout' is the type class that supports the modle widely
  usable class of slicing operations in Numerical.
  for every instance @'RectilinearLayout' form n@, a corresponding
  @'RectOrientationForm' form @, @'RectDownRankForm' form@
  and

-}
class Layout form rank =>
  RectilinearLayout form (rank :: Nat) | form -> rank  where




    unconsOuter:: (S down ~ rank)=> p form -> Shape rank a -> (a, Shape down a)
    consOuter ::  (S down ~ rank)=> p form -> a -> Shape down a -> Shape rank a

    majorAxisSlice :: form -> (Int,Int)-> form  -- should this be maybe form?
    --majorAxisProject :: form -> Int -> form

    majorAxisProject :: (RectilinearLayout downForm subRank
        , rank ~ (S subRank)
        , downForm~ RectDownRankForm form) => form -> Int -> form


    rectlinearSlice :: (RectilinearLayout icForm rank,icForm~InnerContigForm form )=>form -> Index rank -> Index rank -> icForm

class Layout form rank =>  DenseLayout form  (rank :: Nat) | form -> rank  where



    basicToDenseAddress :: form  -> Shape rank Int ->   Address

    basicToDenseIndex :: form -> Address -> Shape rank Int



    basicNextDenseAddress :: form  -> Address ->  Address
    basicNextDenseAddress =  \form shp -> snd
      (basicNextDenseIndex form  $ basicToDenseIndex form  shp )
    {-# INLINE basicNextDenseAddress #-}

    basicNextDenseIndex :: form  -> Shape rank Int ->(Shape rank Int,Address)
    basicNextDenseIndex  = \form shp -> (\ addr ->( basicToDenseIndex form addr, addr) ) $!
       basicNextDenseAddress form  $ basicToDenseAddress form  shp
    {-# INLINE  basicNextDenseIndex #-}


#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 707
    {-# MINIMAL  basicToDenseIndex, basicToDenseAddress,
     (basicNextDenseIndex | basicNextDenseAddress)   #-}
#endif

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
