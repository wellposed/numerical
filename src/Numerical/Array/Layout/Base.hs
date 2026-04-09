{- |  Comments for this modules


-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE CPP #-}

{-# LANGUAGE StandaloneDeriving #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE NoImplicitPrelude #-}

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
  ,SlicedForm
  ,Format
  ,TaggedShape(..)
  ,GDSlice(..)
  ,RSlice(..)
  ,rsIdentity
  ,rsMajorProject
  ,rsMajorSlice
  ,rsFromCorners
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


import Data.Dynamic 
import Data.Data
import Data.Kind(Type)
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
newtype TaggedShape (form :: Type) (rank::Nat) = TaggedShape {unTagShape:: Shape rank Int }
instance Eq (Shape rank Int)=> Eq (TaggedShape f rank) where
  (==) l r =  (==) (unTagShape l) (unTagShape r )

instance Show (Shape rank Int) => Show (TaggedShape f rank) where
  show (TaggedShape ix) =  "TaggedShape (" ++ show ix ++ " )"

instance forall form  rank . (Eq (Shape rank Int),Layout form rank)
  => Ord (TaggedShape form rank) where
  compare left right = compareIndex (Proxy:: Proxy form ) (unTagShape left) (unTagShape right)


-- | Generalized Dense Slice Projection notation (legacy, strided variant).
-- Kept for backwards compatibility.
-- Prefer 'RSlice' for non-strided rectilinear slicing.
data GDSlice (from :: Nat) (to :: Nat) :: Type  where
  GDNil :: GDSlice 'Z 'Z
  GDPick :: Int -> !(GDSlice from to) -> GDSlice ('S from) to
  GDRange :: (Int,Int,Int) {- this is a nonempty interval or error -} -> !(GDSlice from to) -> GDSlice ('S from) ('S to)
  GDAll :: !(GDSlice from to) -> GDSlice ('S from) ('S to)

-- | 'RSlice' is the non-strided rectilinear slice type.
-- It is a morphism @RSlice from to@ that selects a sub-layout of rank @to@
-- from a layout of rank @from@, using only pick (rank reduction),
-- range (sub-interval), and all (identity on one axis).
--
-- This is the right type for 'RectilinearLayout' operations:
--   * @'RSPick' ix rest@ projects out one axis at index @ix@ (rank reduction)
--   * @'RSRange' (lo,hi) rest@ selects an inclusive sub-range on one axis
--   * @'RSAll' rest@ passes one axis through unchanged
--   * @'RSNil'@ is the base case at rank zero
--
-- All of 'majorAxisSlice', 'majorAxisProject', and 'rectSlice'
-- are expressible as applications of 'RSlice'.
data RSlice (from :: Nat) (to :: Nat) :: Type  where
  RSNil   :: RSlice 'Z 'Z
  RSPick  :: {-# UNPACK #-} !Int -> !(RSlice from to) -> RSlice ('S from) to
  RSRange :: {-# UNPACK #-} !Int -> {-# UNPACK #-} !Int
          -> !(RSlice from to) -> RSlice ('S from) ('S to)
  RSAll   :: !(RSlice from to) -> RSlice ('S from) ('S to)


instance Show (GDSlice 'Z 'Z) where
  show _ = "GDNil"

instance (Show (GDSlice (f) ('S t)),Show (GDSlice f t))=> Show (GDSlice ('S f) ('S t)) where
  show (tup `GDRange` rest) = show tup ++ " `GDRange` (" ++ show rest ++ ")"
  show (GDAll rest) =  "GDAll " ++ show rest
  show (ix `GDPick` rest) = show ix ++" `GDPick` " ++ show rest


instance Show (GDSlice f 'Z)=> Show (GDSlice ('S f) 'Z) where
  show (ix `GDPick` rest) = show ix ++" `GDPick` " ++ show rest

-- RSlice Show instances
instance Show (RSlice 'Z 'Z) where
  show RSNil = "RSNil"

instance (Show (RSlice f ('S t)), Show (RSlice f t)) => Show (RSlice ('S f) ('S t)) where
  show (RSRange lo hi rest) = "RSRange " ++ show lo ++ " " ++ show hi ++ " (" ++ show rest ++ ")"
  show (RSAll rest) = "RSAll (" ++ show rest ++ ")"
  show (RSPick ix rest) = "RSPick " ++ show ix ++ " (" ++ show rest ++ ")"

instance Show (RSlice f 'Z) => Show (RSlice ('S f) 'Z) where
  show (RSPick ix rest) = "RSPick " ++ show ix ++ " (" ++ show rest ++ ")"

-- | Build an identity 'RSlice' of a given rank from a 'Shape' witness.
-- @rsAll (3 :* 4 :* Nil)@ yields @RSAll (RSAll RSNil)@.
rsIdentity :: Shape rank a -> RSlice rank rank
rsIdentity Nil = RSNil
rsIdentity (_ :* rest) = RSAll (rsIdentity rest)

-- | Build an 'RSlice' that picks the major (outermost) axis at index @ix@,
-- passing all remaining axes through. This is @majorAxisProject@ as a slice.
rsMajorProject :: Shape rank a -> Int -> RSlice ('S rank) rank
rsMajorProject Nil ix = RSPick ix RSNil
rsMajorProject (_ :* rest) ix = RSPick ix (rsIdentity (_ :* rest))
  where _ = rest -- suppress unused warning; shape only used for rank witness

-- | Build an 'RSlice' that restricts the major axis to @[lo..hi]@ inclusive,
-- passing all remaining axes through. This is @majorAxisSlice@ as a slice.
rsMajorSlice :: Shape rank a -> Int -> Int -> RSlice ('S rank) ('S rank)
rsMajorSlice Nil lo hi = RSRange lo hi RSNil
rsMajorSlice (_ :* rest) lo hi = RSRange lo hi (rsIdentity (_ :* rest))
  where _ = rest

-- | Build a full rectilinear range slice from two corner 'Index' values.
-- Each axis gets @RSRange (lo_i) (hi_i)@.
rsFromCorners :: Index rank -> Index rank -> RSlice rank rank
rsFromCorners Nil Nil = RSNil
rsFromCorners (lo :* loRest) (hi :* hiRest) = RSRange lo hi (rsFromCorners loRest hiRest)

-- | The type family for the result form of applying an 'RSlice' to a format.
-- Implementations should provide instances.
type family SlicedForm (form :: Type) (from :: Nat) (to :: Nat) :: Type



data family Format  lay (contiguity:: Locality)  (rank :: Nat) rep

deriving instance Typeable Format

type family FormatStorageRep ( a:: Type ) :: Type

type instance FormatStorageRep (Format lay ctg rnk rep)= rep

type family  Transposed (form :: Type) :: Type

type family  LayoutAddress (form :: Type) :: Type

-- | every format has a "logical" sibling, that represents the address translation
-- when the underlying buffer layer is contiguous and packed. So it could be claimed
-- that  any type that obeys @a~'LayoutLogicalFormat' a@ is one that can be a legal
-- instance of LayoutBuilder?
type family LayoutLogicalFormat (form :: Type) :: Type

-- | The 'Layout' type class captures the 5-tuple from the layout algebra spec:
--
--   * 'toAddress'  (lkup): index to address (partial)
--   * 'seek':               find first valid entry at or after an index
--   * 'toIndex'   (dec):    address to index (total on valid addresses)
--   * 'nextAddr'  (next):   successor address in enumeration order
--   * 'logicalShape':       the extent of the coordinate space
--
-- Complexity requirements are part of the specification:
--   'toIndex' in O(rank), 'toAddress' in O(rank) for dense / O(rank + log nnz) for sparse,
--   'seek' in O(rank + log n), 'nextAddr' in O(1) amortized.
--
-- Laws:
--   * Inverse: @toIndex form (toAddress form i) == i@ for all @i@ in the domain of 'toAddress'
--   * Seek-lookup: @seek form i == Just (j, a)@ and @j == i@ implies @toAddress form i == Just a@
class Layout form  (rank :: Nat) | form -> rank  where

    -- | The extent of the format's coordinate space.
    logicalShape :: form -> Shape rank Int

    -- | Convert a format into its "contiguous" analogue.
    -- Useful for address translation tricks. Should be O(1) for simple formats.
    logicalForm :: (logicalForm ~ LayoutLogicalFormat form ) => form -> logicalForm

    -- | Transpose the format data type.
    -- Law: @transposedLayout . transposedLayout == id@
    transposedLayout :: (form ~ Transposed transform,transform~Transposed form)=> form  -> transform

    -- | Compare where two (presumably in-bounds) 'Index' values are
    -- in this format's total order.
    compareIndex :: p form-> Shape rank Int ->Shape rank Int -> Ordering

    -- | The (possibly empty) min and max of the valid addresses for a given format.
    addressRange ::  (address ~ LayoutAddress form)=> form -> Maybe (Range address)

    -- | Index to address translation. Returns 'Nothing' for out-of-bounds
    -- or non-manifest indices.
    toAddress :: (address ~ LayoutAddress form)=>
        form  -> Index rank  -> Maybe  address

    -- | Address to index translation. Total on valid addresses.
    -- Behavior on invalid addresses is unspecified.
    -- Complexity: O(rank).
    toIndex ::(address ~ LayoutAddress form)=>
        form -> address -> Index rank

    -- | Compute the next valid address after the given one, or 'Nothing'
    -- if there is no successor. Complexity: O(1) amortized.
    nextAddr :: (address ~ LayoutAddress form)=>
        form  -> address -> Maybe  address

    -- | @'seek' form ix addressHint@ finds the first valid index at or after @ix@.
    -- The 'Maybe address' hint can accelerate the search.
    -- Returns the found index paired with its address, or 'Nothing'.
    -- Complexity: O(rank + log n) where n is the number of valid entries.
    seek :: (address ~ LayoutAddress form)=>
          form  -> Index rank -> Maybe address  -> Maybe ( Index rank, address)


    addressPopCount :: (address ~ LayoutAddress form)=>
        form -> Range address -> Int

    -- | UNSAFE. Convert an address to a raw buffer offset.
    -- Only valid on formats directly backed by a buffer.
    addressAsInt :: (address ~ LayoutAddress form)=>
        form ->  address -> Int
    addressAsInt =
       \ _ _ ->
        error "called addressAsInt on a Layout thats not meant for this world"

    -- | @'affineAddressShift' form addr step@ computes the address @step@ positions
    -- away from @addr@ (positive = forward, negative = backward).
    affineAddressShift :: (address ~ LayoutAddress form) =>
        form -> address -> Int -> Maybe address

    -- | Recover a typed address from a 'Dynamic'. Needed for composite formats
    -- (e.g., zero-copy concatenation of arrays with mixed but compatible formats).
    fromSomeAddress :: (Typeable addr, addr ~ LayoutAddress form ) => p form -> Dynamic -> Maybe addr
    fromSomeAddress _ x = fromDynamic x


    {-# MINIMAL toAddress, toIndex, nextAddr, seek
          , addressRange, logicalShape, compareIndex
          , transposedLayout, addressPopCount, logicalForm, affineAddressShift #-}


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

type family RectDownRankForm   form :: Type

type family InnerContigForm form :: Type

{- | 'RectilinearLayout' supports structure-preserving sub-layout extraction.
  This is a separate axis from the 5-tuple element access in 'Layout'.
  You cannot derive "give me rows 5-10 as a CSR" from toAddress/seek/toIndex/nextAddr;
  slicing is a distinct operation with its own complexity requirements.

  The core method is 'applySlice', which takes an 'RSlice' morphism and
  produces a sub-layout. Convenience functions 'majorAxisSlice',
  'majorAxisProject', and 'rectSlice' are defined outside the class in terms of it.
-}
class Layout form rank =>
  RectilinearLayout form (rank :: Nat) (oriented :: MajorOrientation) | form -> rank oriented where

    -- | Runtime reflection of the orientation.
    formRectOrientation :: p form -> SMajorOrientation oriented

    -- | The rectilinear shape. Equal to 'logicalShape' for untiled layouts.
    -- For tiled layouts, the tile-grid shape.
    -- Complexity: O(rank).
    rectlinearShape :: form -> Index rank

    -- | Decompose a shape into its outermost component and the rest.
    unconsOuter:: ('S down ~ rank)=> p form -> Shape rank a -> (a, Shape down a)

    -- | Prepend a value onto a shape as the outermost component.
    consOuter ::  ('S down ~ rank)=> p form -> a -> Shape down a -> Shape rank a

    -- TODO: applySlice will go here once SlicedForm type family instances
    -- are defined for all formats. For now the old methods remain as the
    -- MINIMAL set so existing instances keep compiling.

    -- | Slice the major (outermost) axis to an inclusive sub-range.
    -- Preserves rank and memory locality.
    -- Complexity: O(1) / O(rank).
    majorAxisSlice :: form -> (Int,Int)-> form

    -- | Project the major axis at a single index, reducing rank by one.
    -- Complexity: O(1) / O(rank).
    majorAxisProject :: (RectilinearLayout downForm subRank oriented,
     rank ~ ('S subRank) , downForm~ RectDownRankForm form) => form -> Int -> downForm

    -- | Non-strided rectilinear slice from two corner indices (inclusive).
    -- For dense: O(rank). For sparse: O(outer dimension size).
    rectSlice :: (RectilinearLayout icForm rank oriented,icForm~InnerContigForm form )
              => form -> Index rank -> Index rank -> icForm


{- | 'DenseLayout' has instances only for dense array formats.
  These operations provide direct address arithmetic that bypasses
  the Maybe-returning 'toAddress' / 'nextAddr'.
  Intended for optimization, not as a default API.
-}
class Layout form rank =>  DenseLayout form  (rank :: Nat) | form -> rank  where

    toDenseAddress :: form  -> Index rank  ->   Address

    toDenseIndex :: form -> Address -> Index rank

    nextDenseAddress :: form  -> Address ->  Address
    nextDenseAddress =  \form shp -> snd
      (nextDenseIndex form  $ toDenseIndex form  shp )
    {-# INLINE nextDenseAddress #-}

    nextDenseIndex :: form  -> Index rank ->(Index rank ,Address)
    nextDenseIndex  = \form shp -> (\ addr ->( toDenseIndex form addr, addr) ) $!
       nextDenseAddress form  $ toDenseAddress form  shp
    {-# INLINE  nextDenseIndex #-}

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 707
    {-# MINIMAL  toDenseIndex, toDenseAddress,
     (nextDenseIndex | nextDenseAddress)   #-}
#endif

{-
*Numerical.Array.Layout> toAddress (FormColumn (2 :* 3 :* 7 :* Nil)) (0:* 2 :* 2 :* Nil)
Address 16
*Numerical.Array.Layout> toAddress (FormColumn (2 :* 3 :* 7 :* Nil)) (1:* 0 :* 0 :* Nil)
Address 1
*Numerical.Array.Layout> toAddress (FormColumn (2 :* 3 :* 7 :* Nil)) (0:* 0 :* 0 :* Nil)
Address 0
*Numerical.Array.Layout> toAddress (FormColumn (2 :* 3 :* 7 :* Nil)) (0:* 1 :* 0 :* Nil)
Address 2
*Numerical.Array.Layout> toAddress (FormColumn (2 :* 3 :* 7 :* Nil)) (0:* 0 :* 1 :* Nil)



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
