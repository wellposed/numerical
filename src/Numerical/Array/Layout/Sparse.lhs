
the following (currently 5) sparse formats will live here


DirectSparse 1dim



one subtlety and a seemingly subtle point will be
that contiguous / inner contiguous sparse arrays
in  2dim  (and  1dim) will have an ``inner dimension" shift int.
This is so that slices can  be zero copy on *BOTH* the array of values,
and the Format indexing array machinery.

Note that in the 2dim case, it still wont quite be zero copy, because the
offsets into the inner dimension lookup table (not quite the right word)
will have to change when a general slice is used rather than a slice
that acts only on the outermost dimension.

\begin{code}


-- {-# LANGUAGE PolyKinds   #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE StandaloneDeriving#-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE FlexibleContexts #-}

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 707
 {-# LANGUAGE AutoDeriveTypeable #-}
#endif
module Numerical.Array.Layout.Sparse(
  SparseLayout(..)
  ,Layout(..)
  ,DirectSparse
  ,CSR
  ,CSC
  ,CompressedSparseRow
  ,CompressedSparseColumn
  ,Format(..)
  ,StorageVector
  ) where

import Data.Data
import Data.Bits (unsafeShiftR)
import Control.Applicative
import Numerical.Array.Layout.Base
import Numerical.Array.Shape
import Numerical.Array.Address
import qualified  Data.Vector.Generic as V

data CompressedSparseRow
  deriving Typeable
type CSR = CompressedSparseRow

data CompressedSparseColumn
    deriving Typeable

type CSC = CompressedSparseColumn

data DirectSparse
    deriving Typeable



data instance Format DirectSparse  Contiguous (S Z) rep =
    FormatDirectSparseContiguous {
      logicalShapeDirectSparse:: {-# UNPACK#-} !Int
      ,logicalBaseIndexShiftDirectSparse::{-# UNPACK#-} !Int
      ,indexTableDirectSparse :: ! ((StorageVector rep) Int )  }
    --deriving (Show,Eq,Data)



{-
for some listings of the design space of Sparse matrices
as found in other tools,
see < https://software.intel.com/sites/products/documentation/doclib/mkl_sa/11/mklman/GUID-9FCEB1C4-670D-4738-81D2-F378013412B0.htm >

<  http://netlib.org/linalg/html_templates/node90.html > is also pretty readable

theres a subtle detail about the invariants of contiguous vs inner inner contiguous
for CSR and CSC
when I do an inner contiguous / contiguous slice / projection,
what "address shifts" do i need to track to make sure the slices
are zero copy as much as possible

just slicing on the outer dimension doesn't need any row shifts,
but a generalized (a,b) ... (a+x,b+y) selection when a,b!=0 does need a inner
dim shift,

NOTE that translating the inner dimension table's addresses to the corresponding
value buffer's address can require a shift!
This will happen when doing a MajorAxis (outer dimension) slice
the picks out a Suffix of the CSR matrix's rows


note that there are 2 formulations of CSR (/ CSC) formats

a) 3 array: value, column index,  and  row start vectors

b) 4 array: value, column index, rowstart, and row end vectors

lets use choice a) for contiguous vectors, and choice b) for
inner contiguous vectors.

In both cases we need to enrich the type with a "buffer shift"
to handle correctly doing lookups on submatrices picked out
by either a major axis slice

-}


--deriving instance (Show (Shape (S (S Z)) Int), Show (StorageVector rep Int) )
    -- => Show (Format CompressedSparseRow Contiguous (S (S Z)) rep)

--deriving instance  (Eq (Shape (S (S Z)) Int), Eq (StorageVector rep Int) )
    -- => Eq (Format CompressedSparseRow Contiguous (S (S Z)) rep)

--deriving instance (Data (Shape (S (S Z)) Int), Data (StorageVector rep Int) )
  --- => Data (Format CompressedSparseRow Contiguous (S (S Z)) rep)

--deriving instance  (Typeable (Shape (S (S Z)) Int ), Typeable (StorageVector rep Int) )
 -- => Typeable (Format CompressedSparseRow Contiguous (S (S Z)) rep)
    --deriving (Eq,Data,Typeable)


{-
NOTE!!!!!
logicalValueBufferAddressShiftContiguousCSR (and friends)
are so that major axis slices can still use the same buffer,
(needed for both Contiguous and InnerContiguous cases).
So When looking up the Address for a value based upon its
Inner dimension, we need to *SUBTRACT* that shift
to get the correct offset index into the current SLICE.

Phrased differently, This address shift is the *Discrepancy/Difference*
between the size of the elided prefix of the Vector and the starting


This is kinda a good argument for not punting the Slicing on the raw buffers to
Vector, because it generally makes this a bit more subtle to think about
and someone IS going to implement something wrong this way!


Another subtle and potentially confusing point is distinguishing between
Affine shifts in the Index Space vs the Address space.

Only the outer dimension lookup table shift is needed in the Contiguous
2dim case, but the 2dim InnerContiguous case is a bit more confusing
because of the potential for a slice along the inner dimension

Rank 1 sparse  (like Direct sparse) is only Contiguous,
and either a) doesn't need a shift, or b) only needs an index shift
commensurate matching the leading implicit index of a Major Axis Slice


-}

data instance Format CompressedSparseRow Contiguous (S (S Z)) rep =
    FormatContiguousCompressedSparseRow {
      logicalRowShapeContiguousCSR ::  {-# UNPACK #-} !Int
      ,logicalColumnShapeContiguousCSR ::  {-# UNPACK #-} !Int
      ,logicalValueBufferAddressShiftContiguousCSR:: {-# UNPACK #-} !Int
      ,logicalColumnIndexContiguousCSR :: !(StorageVector rep Int)
      ,logicalRowStartIndexContiguousCSR :: ! (StorageVector rep Int )
  }


data instance Format CompressedSparseRow InnerContiguous (S (S Z)) rep =
    FormatInnerContigCompressedSparseRow {
      logicalRowShapeInnerContigCSR ::    {-# UNPACK #-} !Int
      ,logicalColumnShapeInnerContigCSR ::  {-# UNPACK #-} !Int
      ,logicalValueBufferAddressShiftInnerContigCSR:: {-# UNPACK #-} !Int
      ,logicalColumnIndexInnerContigCSR :: !(StorageVector rep Int)
      ,logicalRowStartIndexInnerContigCSR :: ! (StorageVector rep Int )
      ,logicalRowEndIndexInnerContigCSR :: ! (StorageVector rep Int )
  }
      --deriving (Show,Eq,Data)

data instance Format CompressedSparseColumn Contiguous (S (S Z)) rep =
    FormatContiguousCompressedSparseColumn {
      logicalRowShapeContiguousCSC ::   {-# UNPACK #-} !Int
      ,logicalColumnShapeContiguousCSC ::   {-# UNPACK #-} !Int
      ,logicalValueBufferAddressShiftContiguousCSC:: {-# UNPACK #-} !Int
      ,logicalRowIndexContiguousCSC :: !(StorageVector rep Int)
      ,logicalColumnStartIndexContiguousCSC :: ! (StorageVector rep Int )
  }
    --deriving (Show,Eq,Data)

data instance Format CompressedSparseColumn InnerContiguous (S (S Z)) rep =
    FormatInnerContigCompressedSparseColumn {
      logicalRowShapeInnerContigCSC ::    {-# UNPACK #-} !Int
      ,logicalValueBufferAddressShiftInnerContigCSC:: {-# UNPACK #-} !Int
      ,logicalRowIndexInnerContigCSC :: !(StorageVector rep Int)
      ,logicalColumnStartIndexInnerContigCSC :: ! (StorageVector rep Int )
      ,logicalColumnEndIndexInnerContigCSC :: ! (StorageVector rep Int )
  }
    --deriving (Show,Eq,Data)



class Layout form rank  => SparseLayout form  (rank :: Nat)  | form -> rank where

    type SparseLayoutAddress form :: *

    leastSparseAddress ::  (address ~ SparseLayoutAddress form)=> form -> address

    greatestSparseAddress ::  (address ~ SparseLayoutAddress form)=> form -> address

    basicToSparseAddress :: (address ~ SparseLayoutAddress form)=>
        form  -> Shape rank Int -> Maybe  address


    basicToSparseIndex ::(address ~ SparseLayoutAddress form)=>
        form -> address -> Shape rank Int


    basicNextAddress :: (address ~ SparseLayoutAddress form)=>
        form  -> address -> Maybe  address

    {-# INLINE basicNextIndex #-}
    basicNextIndex :: form  -> Shape rank Int -> Maybe  (Shape rank Int)
    basicNextIndex =
        \ form shp ->
          basicToSparseAddress form shp >>=
            (\x ->  fmap (basicToSparseIndex form)  $  basicNextAddress form x)

    {-# MINIMAL basicToSparseAddress, basicToSparseIndex, basicNextAddress
      ,greatestSparseAddress, leastSparseAddress #-}

\end{code}



CSR and CSC go here, and their version of lookups and next address and next index




\begin{code}

--  Offset binary search --- cribbed with permission from
-- edward kmett's structured lib
--
-- now assumed each key is unique and ordered
--
-- Assuming @l <= h@. Returns @h@ if the predicate is never @True@ over @[l..h)@
search :: (Int -> Ordering) -> Int -> Int -> Int
search p = go where
  go l h
    | l == h    = l
    | otherwise = case p m of
                  LT -> go (m+1) h
                  ---  entry is less than target, go up!
                  EQ -> m
                  -- we're there! Finish early
                  GT -> go l m
                  -- entry is greater than target, go down!
    where hml = h - l
          m = l + unsafeShiftR hml 1 + unsafeShiftR hml 6
{-# INLINE search #-}

lookupExact :: (Ord k, V.Vector vec k) => vec k -> k -> Maybe Int
lookupExact ks key
  | j <- search (\i -> compare (ks V.! i)  key) 0 (V.length ks - 1)
  , ks V.! j == key = Just $! j
  | otherwise = Nothing
{-# INLINE lookupExact #-}

lookupExactRange :: (Ord k, V.Vector vec k) => vec k -> k -> Int -> Int -> Maybe Int
lookupExactRange  ks key lo hi
  | j <- search (\i -> compare (ks V.! i)  key) lo hi
  , ks V.! j == key = Just $! j
  | otherwise = Nothing
{-# INLINE lookupExactRange  #-}

--lookupLUB ::  (Ord k, V.Vector vec k) => vec k -> k -> Maybe Int
--lookupLUB  ks key
--  | j <- search  (\i -> compare (ks V.! i)  key) 0 (V.length ks - 1)
--  , ks V.! j <= key = Just $! j
--  | otherwise = Nothing
--{-# INLINE lookupLUB  #-}

type instance  Transposed (Format DirectSparse Contiguous (S Z) rep )=
   (Format DirectSparse Contiguous (S Z) rep )

instance Layout   (Format DirectSparse Contiguous (S Z) rep ) (S Z) where
  transposedLayout  = id
  {-# INLINE transposedLayout #-}
  basicFormShape = \ form -> logicalShapeDirectSparse form  :* Nil
  {-# INLINE basicFormShape #-}
  basicCompareIndex = \ _ (a:* Nil) (b :* Nil) ->compare a b
  {-# INLINE basicCompareIndex#-}

instance V.Vector (StorageVector rep) Int
   => SparseLayout  (Format DirectSparse Contiguous (S Z) rep ) (S Z) where
      type SparseLayoutAddress (Format DirectSparse Contiguous (S Z) rep) =  Address

      leastSparseAddress = \ _ -> Address 0

      greatestSparseAddress =
        \ (FormatDirectSparseContiguous _ _   lookupTable)->
            Address (V.length lookupTable - 1 )

-- TODO, double check that im doing shift correctly
      {-# INLINE basicToSparseAddress #-}
      basicToSparseAddress =
          \ (FormatDirectSparseContiguous shape  indexshift lookupTable) (ix:*_)->
             if  not (ix < shape && ix > 0 ) then  Nothing
              else  fmap Address  $! lookupExact lookupTable (ix + indexshift)

      {-# INLINE basicToSparseIndex #-}
      basicToSparseIndex =
        \ (FormatDirectSparseContiguous _ shift lut) (Address addr) ->
            ((lut V.! addr ) - shift) :* Nil

      {-# INLINE basicNextAddress #-}
      basicNextAddress =
        \ (FormatDirectSparseContiguous _ _ lut) (Address addr) ->
          if  addr >= (V.length lut) then Nothing else Just  (Address (addr+1))
------------
------------

type instance Transposed (Format CompressedSparseRow Contiguous (S (S Z)) rep )=
    (Format CompressedSparseColumn Contiguous (S (S Z)) rep )

instance Layout (Format CompressedSparseRow Contiguous (S (S Z)) rep ) (S (S Z)) where
  transposedLayout  = \(FormatContiguousCompressedSparseRow a b c d e) ->
    (FormatContiguousCompressedSparseColumn a b c d e )
  {-# INLINE transposedLayout #-}
  basicFormShape = \ form -> logicalRowShapeContiguousCSR form  :*
         logicalColumnShapeContiguousCSR form :* Nil
  {-# INLINE basicFormShape #-}
  basicCompareIndex = \ _ as  bs ->shapeCompareRightToLeft as bs
  {-# INLINE basicCompareIndex#-}

instance  (V.Vector (StorageVector rep) Int )
  => SparseLayout (Format CompressedSparseRow Contiguous (S (S Z)) rep ) (S (S Z)) where

      type SparseLayoutAddress (Format CompressedSparseRow Contiguous (S (S Z)) rep ) = SparseAddress

      {-# INLINE leastSparseAddress #-}
      leastSparseAddress = \_ -> SparseAddress 0 0

      {-# INLINE greatestSparseAddress#-}
      greatestSparseAddress  =
       \ (FormatContiguousCompressedSparseRow _ y_range _
          columnIndex _) ->
              SparseAddress (y_range - 1) (V.length columnIndex - 1 )


      {-#INLINE basicToSparseIndex #-}
      basicToSparseIndex =
       \ (FormatContiguousCompressedSparseRow _ _  _ columnIndex _)
          (SparseAddress outer inner) -> (columnIndex V.! inner ) :* outer :*  Nil
          -- outer is the row (y index) and inner is the lookup position for the x index


{-
theres 3 cases for contiguous next address:
in the middle of a run on a fixed outer dimension,
need to bump the outer dimension, or we're at the end of the entire array

we make the VERY strong assumption that no illegal addresses are ever made!

note that for very very small sparse matrices, the branching will have some
overhead, but in general branch prediction should work out ok.
-}
      {-# INLINE basicNextAddress #-}
      basicNextAddress =
         \ (FormatContiguousCompressedSparseRow _ _ _
                                                columnIndex rowstartIndex)
            (SparseAddress outer inner) ->
              if not  (inner == (V.length columnIndex -1)
                                          {- && outer == (y_range-1) -}
                     || (inner +1) == (rowstartIndex V.! (outer + 1)))
                then
                  Just (SparseAddress outer (inner+1))
                else
                  if inner == (V.length columnIndex -1)
                    then Nothing
                    else Just (SparseAddress (outer + 1) (inner + 1 ) )

        --  error "finish me damn it"
      {-# INLINE basicToSparseAddress #-}
      basicToSparseAddress =
        \ (FormatContiguousCompressedSparseRow x_range y_range addrShift
              columnIndex rowstartIndex)
          (ix_x:*ix_y :* _ ) ->
            if  not (ix_x >= x_range ||  ix_y >=y_range )
              then
              -- slightly different logic when ix_y < range_y-1 vs == range_y-1
              -- because contiguous, don't need the index space shift though!
                       SparseAddress ix_y   <$>
                          lookupExactRange columnIndex ix_x ((rowstartIndex V.! ix_y) - addrShift)
                            (if  ix_y < (y_range-1)
                              -- addr shift is for correcting from a major axis slice
                              then  (rowstartIndex V.! (ix_y+1) ) - addrShift
                              else V.length columnIndex  - 1 )
              else   (Nothing :: Maybe SparseAddress )

\end{code}
