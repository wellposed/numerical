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
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE CPP #-}

{-# LANGUAGE StandaloneDeriving #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 707
 {-# LANGUAGE AutoDeriveTypeable #-}
#endif


module Numerical.Array.Layout.Base(
  Layout(..)
  ,DenseLayout(..)
  ,RectilinearLayout(..)
  ,LayoutAddress
  ,LayoutLogicalFormat
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
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 709
import  qualified Control.Applicative as A
import Prelude hiding (foldr,foldr1,foldl1,foldl,map)
import  qualified  Data.Foldable as F
#elif __GLASGOW_HASKELL__ >= 709
import  qualified Control.Applicative as A
import  qualified  Data.Foldable as F
#endif

#if MIN_VERSION_base(4,8,0)
import Prelude hiding (foldl)
#endif

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
-- This provides a type safe interface for the classical
-- general array slice notation.
-- That said, its only useful for dense array formats,
-- at least in general. For formats that aren't "rectilinear dense",
-- this COULD be used as a description format for traversing
-- over various rectilinear subsets of points though?
data GDSlice (from :: Nat) (to :: Nat) :: *  where
  GDNil :: GDSlice 'Z 'Z
  GDPick :: Int -> GDSlice from to -> GDSlice ('S from) to
  GDRange :: (Int,Int,Int) {- this is a nonempty interval or error -} -> GDSlice from to -> GDSlice ('S from) ('S to)
  GDAll :: GDSlice from to -> GDSlice ('S from) ('S to)

{-
TODO: for things that

-}


instance Show (GDSlice 'Z 'Z) where
  show _ = "GDNil"

instance (Show (GDSlice (f) ('S t)),Show (GDSlice f t))=> Show (GDSlice ('S f) ('S t)) where
  show (tup `GDRange` rest) = show tup ++ " `GDRange` (" ++ show rest ++ ")"
  show (GDAll rest) =  "GDAll " ++ show rest
  show (ix `GDPick` rest) = show ix ++" `GDPick` " ++ show rest

instance Show (GDSlice f 'Z)=> Show (GDSlice ('S f) 'Z) where
  show (ix `GDPick` rest) = show ix ++" `GDPick` " ++ show rest
--instance Show (GDSlice f t)  where
--  func =

{-
In some (moderately precise sense)

-}


-- GDRange (from,step,to)
  -- GDAll is just sugar for a special case of GDRange, but maybe its worthwhile sugar?

--computeSlicePlan:: GDSlice from to -> Shape from Int -> Shape from (Either Int (AffineRange Int))
--computeSlicePlan GDNil  Nil = Nil
--computeSlicePlan  ( ix `GDPick` gdRest )
--                  (bd:* shpRest)| ix < bd   && ix >= 0 = Left ix :* computeSlicePlan gdRest shpRest
--                      | otherwise = error
--                          $ "bad indices for computeSlicePlan " ++ show (ix,bd)
--computeSlicePlan ( (strt,step,end) `GDRange` grest) (bd:* shprest)



data family Format  lay (contiguity:: Locality)  (rank :: Nat) rep

deriving instance Typeable Format

type family FormatStorageRep ( a:: * ) :: *

type instance FormatStorageRep (Format lay ctg rnk rep)= rep

type family  Transposed (form :: *) :: *

type family  LayoutAddress (form :: *) :: *

-- TODO / FIXME remove the basic* prefix  from all the operations
-- this was done originally because

-- | every format has a "logical" sibling, that represents the address translation
-- when the underlying buffer layer is contiguous and packed. So it could be claimed
-- that  any type that obeys @a~'LayoutLogicalFormat' a@ is one that an be a legal
-- instance of LayoutBuilder?
type family LayoutLogicalFormat (form :: *) :: *

-- | The 'Layout' type class
class Layout form  (rank :: Nat) | form -> rank  where

    -- | 'basicLogicalShape' gives the extent of the format.
    -- | For example, for a matrix (rank-2 array), returns
    -- | 'numRows :* numColumns :* Nil'.
    basicLogicalShape :: form -> Shape rank Int

    -- | 'basicLogicalForm' converts a given format into its "contiguous" analogue
    -- this is useful for supporting various address translation manipulation tricks
    -- efficiently. Note that any valid  simple format should strive to ensure this is an O(1) operation.
    -- though certain composite 'Layout' instances may provide a slower implementation.
    basicLogicalForm :: (logicalForm ~ LayoutLogicalFormat form ) => form -> logicalForm


    -- | 'transposedLayout' transposes the format data type
    transposedLayout :: (form ~ Transposed transform,transform~Transposed form)=> form  -> transform

    -- | 'basicCompareIndex' lets you compare where two (presumably inbounds)
    -- 'Index' values are in a formats ordering. The logical 'Shape' of the array
    -- is not needed
    basicCompareIndex :: p form-> Shape rank Int ->Shape rank Int -> Ordering

    -- | the (possibly empty) min and max of the valid addresses for a given format.
    -- @minAddress = fmap _RangeMin . rangedFormatAddress@
    -- and @maxAddress = fmap _RangeMax . rangedFormatAddress@
    -- FIXME : This also is a terrible name
    basicAddressRange ::  (address ~ LayoutAddress form)=> form -> Maybe (Range address)

    -- | 'indexToAddress' takes an Index, and tries to translate it to an address if its in bounds
    --
    indexToAddress :: (address ~ LayoutAddress form)=>
        form  -> Index rank  -> Maybe  address

    -- | 'basicToIndex' takes an address, and always successfully translates it to
    -- a valid index. Behavior of invalid addresses constructed by a library user
    -- is unspecified.
    addressToIndex ::(address ~ LayoutAddress form)=>
        form -> address -> Index rank

    -- | 'basicNextAddress' takes an address, and tries to compute the next valid
    -- address, or returns Nothing if there is no subsequent valid address.
    basicNextAddress :: (address ~ LayoutAddress form)=>
        form  -> address -> Maybe  address

    -- |  @'basicNextIndex' form ix mbeAddress@  computes the next valid index after
    -- @ix@ if it exists. It takes a @'Maybe' address@ as a hint for where to do the search for the successor.
    -- If the index is in bounds and not the last index, it returns both the index and the associated address.
    basicNextIndex :: (address ~ LayoutAddress form)=>
          form  -> Index rank -> Maybe address  -> Maybe ( Index rank, address)


    basicAddressPopCount :: (address ~ LayoutAddress form)=>
        form -> Range address -> Int

    -- | This operation is REALLY unsafe
    -- This should ONLY be used on Formats that are directly
    -- paired with a Buffer or Mutable Buffer (ie a Vector)
    --  This operation being in this class is also kinda a hack
    -- but lets leave it here for now
    basicAddressAsInt :: (address ~ LayoutAddress form)=>
        form ->  address -> Int
    basicAddressAsInt =
       \ _ _ ->
        error "called basicAddressAsInt on a Layout thats not meant for this world"

    -- | The semantics of @`basicAffineAddressShift` form addr step@ is that
    -- when  step > 0, its equivalent to iteratively computing 'basicNextAddress' @step@ times.
    -- However, the step size can be negative, which means it can
    basicAffineAddressShift :: (addres ~ LayoutAddress form) =>
        form -> address -> Int -> Maybe address


    {-# MINIMAL indexToAddress, indexToAddress, basicNextAddress,basicNextIndex
          ,basicAddressRange,basicLogicalShape,basicCompareIndex
          , transposedLayout, basicAddressPopCount,basicLogicalForm, basicAffineAddressShift #-}


{- |
these names aren't ideal, but lets punt on bikeshedding till theres >= 2 serious
users
-}
data MajorOrientation = Rowed | Columned | BlockedColumn | BlockedRow
  deriving(Data,Typeable)

data SMajorOrientation (o :: MajorOrientation) where
    SRowed :: SMajorOrientation 'Rowed
    SColumned :: SMajorOrientation 'Columned
    SBlockedRow :: SMajorOrientation 'BlockedRow
    SBlockedColumn :: SMajorOrientation 'BlockedColumn


-- |  Every instance of 'RectilinearLayout' needs to have a corresponding
-- 'RectOrientationForm', 'RectDownRankForm', and 'InnerContigForm'
type family RectOrientationForm form :: MajorOrientation

type family RectDownRankForm   form :: *

type family InnerContigForm form :: *

{- | 'RectilinearLayout' is the type class that supports the modle widely
  usable class of slicing operations in Numerical.
  for every instance @'RectilinearLayout' format rank orientation@, a corresponding
  @'RectOrientationForm' form @, @'RectDownRankForm' form@
  and @'InnerContigForm' form@ type family instance should be defined

  The purpose of 'RectilinearLayout' class is to provide

-}
class Layout form rank =>
  RectilinearLayout form (rank :: Nat) (oriented :: MajorOrientation) | form -> rank oriented where

    -- | 'formRectOrientation' provides a runtime mechanism for reflecting
    -- the orientation of the format
    formRectOrientation :: p form -> SMajorOrientation oriented

    -- | For  @'rectlinearShape' form==shp@, we always have that
    -- @'basicLogicalShape' form  `weaklyDominates` shp@.
    -- when 'strictlyDominates' holds, that implies that the underlying array format
    -- is a rectilinear layout whose "elements" are tiles of a fixed size array format.
    -- For this initial release and initial set of applicable rectilinear array formats,
    -- the following is always true @'basicLogicalShape' form  == basicLogicalShape' form @
    -- Should be @O(1)@ always. Or more precisely @O(rank)@
    rectlinearShape :: form -> Index rank

    unconsOuter:: ('S down ~ rank)=> p form -> Shape rank a -> (a, Shape down a)
    consOuter ::  ('S down ~ rank)=> p form -> a -> Shape down a -> Shape rank a

    -- | @'majorAxisSlice' fm (x,y)@ requires that y-x>=1, ie that more than
    -- one sub range wrt the major axis be selected, so that the logical
    -- rank of the selected array stays the same. This operation also preserves
    -- memory locality as applicable.
    -- @O(1)@ / @O(rank)@
    majorAxisSlice :: form -> (Int,Int)-> form
     -- should this be -> Maybe form?


    -- | @'majorAxixProject' form x@ picks a "row" with respect to the outer most
    -- dimension of the array format. This will
    -- @O(1)@ or @O(rank)@
    majorAxisProject :: (RectilinearLayout downForm subRank oriented,
     rank ~ ('S subRank) , downForm~ RectDownRankForm form) => form -> Int -> downForm

    -- | this is the nonstrided subset of general array slice notation.
    --  Invoke as @'rectilinearSlice'  form  leastCorner greatestCorner@,
    -- where the least and greatest corners of the sub array are determined
    -- by the 'strictlyDominates' partial order on the bounds of the sub array.
    -- For Dense array formats, this should be @O(1)@ or more precisely @O(rank)@
    -- For the basic Sparse array formats thus far the complexity should be
    -- @O(size of outermost dimension)@, which could be computed by
    --  @fst . unconsOuter [form] . rectilinearShape $ form@
    rectlinearSlice :: (RectilinearLayout icForm rank oriented,icForm~InnerContigForm form )=>form -> Index rank -> Index rank -> icForm -- FIXME, need the range infos????? (icfFOrm, adddress,address)


{- | 'DenseLayout' only has instances for Dense array formats.
this class will need some sprucing up for the beta,
but its ok for now. NB that 'DenseLayout' is really strictly meant to be used
for optimization purposes, and not meant as a default api
-}
class Layout form rank =>  DenseLayout form  (rank :: Nat) | form -> rank  where



    basicToDenseAddress :: form  -> Index rank  ->   Address

    basicToDenseIndex :: form -> Address -> Index rank



    basicNextDenseAddress :: form  -> Address ->  Address
    basicNextDenseAddress =  \form shp -> snd
      (basicNextDenseIndex form  $ basicToDenseIndex form  shp )
    {-# INLINE basicNextDenseAddress #-}

    basicNextDenseIndex :: form  -> Index rank ->(Index rank ,Address)
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
