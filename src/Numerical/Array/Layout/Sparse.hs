{-
The following (currently 5) sparse formats will live here:
  * Direct sparse
  * Compressed sparse row, contiguous
  * Compressed sparse row, inner contiguous
  * Compressed sparse column, contiguous
  * Compressed sparse column, inner contiguous

See https://en.wikipedia.org/wiki/Sparse_matrix for an overview on these formats, or see the links posted in the later comments.


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
-}



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
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 707
 {-# LANGUAGE AutoDeriveTypeable #-}
#endif
module Numerical.Array.Layout.Sparse(
  Layout(..)
  ,DirectSparse
  ,CSR
  ,CSC
  ,CompressedSparseRow
  ,CompressedSparseColumn --  FIX ME, re add column support later
  ,Format(FormatDirectSparseContiguous
      ,FormatContiguousCompressedSparseRow
      ,FormatInnerContiguousCompressedSparseRow
      ,FormatContiguousCompressedSparseColumn
      ,FormatInnerContiguousCompressedSparseColumn)
  ,ContiguousCompressedSparseMatrix(..)
  ,InnerContiguousCompressedSparseMatrix(..)
  ,module Numerical.Array.Layout.Base
  ) where

import Data.Data
import Data.Bits (unsafeShiftR)
import Control.Applicative
import Numerical.Array.Layout.Base
--import Numerical.Array.Shape
import Numerical.InternalUtils
import qualified  Data.Vector.Generic as V
import Prelude hiding (error )


data CompressedSparseRow
  deriving (Typeable)

type CSR = CompressedSparseRow

data CompressedSparseColumn
    deriving (Typeable)

type CSC = CompressedSparseColumn

data DirectSparse
    deriving (Typeable)



-- | The "base case" of our sparse arrays. That is, this can only hold rank-1
-- | arrays. This is kind-of in-between "List of lists (LIL)" and "Coordinate list (COO)".
data instance Format DirectSparse 'Contiguous ('S 'Z) rep =
  FormatDirectSparseContiguous
  { _dsLogicalShape          :: {-# UNPACK#-} !Int -- ^| number of nonzero entries
  , _dsLogicalBaseIndexShift :: {-# UNPACK#-} !Int -- TODO I don't understand why you would need an offset for this. Even if this is a subarray of some larger array, why not just make _dsIndexTable point to the right offset in the array?
  , _dsIndexTable            :: !(BufferPure rep Int)
  }


deriving instance Show  (BufferPure rep Int )  => Show (Format DirectSparse 'Contiguous ('S 'Z) rep)



{-
for some listings of the design space of Sparse matrices
as found in other tools,
see < https://software.intel.com/en-us/mkl_11.2_ref >
and then navigate to the section  "Sparse Matrix Storage Formats"  within
"BLAS and Sparse BLAS Routines"

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


--deriving instance (Show (Shape (S (S Z)) Int), Show (BufferPure rep Int) )
    -- => Show (Format CompressedSparseRow Contiguous (S (S Z)) rep)

--deriving instance  (Eq (Shape (S (S Z)) Int), Eq (BufferPure rep Int) )
    -- => Eq (Format CompressedSparseRow Contiguous (S (S Z)) rep)

--deriving instance (Data (Shape (S (S Z)) Int), Data (BufferPure rep Int) )
  --- => Data (Format CompressedSparseRow Contiguous (S (S Z)) rep)

--deriving instance  (Typeable (Shape (S (S Z)) Int ), Typeable (BufferPure rep Int) )
 -- => Typeable (Format CompressedSparseRow Contiguous (S (S Z)) rep)
    --deriving (Eq,Data,Typeable)


{-
NOTE!!!!!
_logicalBaseIndexShiftDirectSparse (and friends)
are so that major axis slices can still use the same buffer,
(needed for both Contiguous and InnerContiguous cases).
So When looking up the Address for a value based upon its
Inner dimension, we need to *SUBTRACT* that shift
to get the correct offset index into the current SLICE.

NB: THIS IS A TERRRIBLE EXPLANATION, FIXMEEEEE

Phrased differently, This address shift is the *Discrepancy/Difference*
between the size of the elided prefix of the Vector and the starting
position of the manifest entries.

(Q: does this ever ever matter, or can i punt that to vector, and only
need this )


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


theres a BIG corner case in most standard CSR / CSC formats which is
underspecified in most docs about CSC and CSR formats.
Consider Without loss of generality, CSR format
  1) how are empty rows modeled/signaled?
  2) if the last row is empty, how is that signaled?

2) The last row is signaled to be be empty by having
  the last entry of _outerDim2InnerDim buffer be set to >=
  length of _innerDimIndex buffer (ie >= 1 + largest index of _innerDimIndex)
1)

note that the outer index table has 1+#rows length, with the last one being the
length of the array

-}

-- does this need the index space shift for outer range slices???

-- | Basic CSR/CSC (compressed sparse row/column) format. To use this as CSR, make
-- | rows the outer dim; to use as CSC, make columns the outer-dim.

-- | IA and JA are the names of the matrices used in
-- | https://en.wikipedia.org/wiki/Sparse_matrix#Compressed_sparse_row_.28CSR.2C_CRS_or_Yale_format.29.
-- |
-- | Note that, unlike the description on Wikipedia, A itself is not here! This
-- | doesn't actually store the values of the array, it is just metadata about
-- | how the matrix is stored.
data ContiguousCompressedSparseMatrix rep =
  FormatContiguousCompressedSparseInternal
  { _ccsmOuterDim :: {-# UNPACK #-} !Int -- ^| length of IA (i.e. if we are row-major, then this is the number of rows)
  , _ccsmInnerDim :: {-# UNPACK #-} !Int -- ^| length of JA (this is the number of nonzero elements in the array)
  , _ccsmInnerDimIndex     :: !(BufferPure rep Int) -- ^| JA
  , _ccsmOuterDim2InnerDim :: !(BufferPure rep Int) -- ^| IA
  }
  deriving (Typeable)

deriving instance (Show (BufferPure rep Int))=> Show (ContiguousCompressedSparseMatrix rep)

{-
  outerDim innerDim  innerTable  outer2InnerStart
-}



{-
for Row major Compressed Sparse (CSR)
the X dim (columns) are the inner dimension, and Y dim (rows) are the outer dim
-}



data  InnerContiguousCompressedSparseMatrix rep =
  FormatInnerContiguousCompressedSparseInternal
  { _iccsmOuterDimInner           :: {-# UNPACK #-} !Int
  , _iccsmInnerDimInner           :: {-# UNPACK #-} !Int
  , _iccsmInnerDimIndexShiftInner :: {-# UNPACK #-} !Int
  , _iccsmInnerDimIndexInner          :: !(BufferPure rep Int)
  , _iccsmOuterDim2InnerDimStartInner :: !(BufferPure rep Int)
  , _iccsmOuterDim2InnerDimEndInner   :: !(BufferPure rep Int) -- TODO Not sure what the point of this is. Can't we infer that outerDim2InnerDimEnd[k] = outerDim2InnerDimStart[k + 1] - 1?
  }
  deriving Typeable

deriving instance (Show (BufferPure rep Int))=> Show (InnerContiguousCompressedSparseMatrix rep)


newtype instance Format CompressedSparseRow 'Contiguous ('S ('S 'Z)) rep =
    FormatContiguousCompressedSparseRow {
      _getFormatContiguousCSR :: (ContiguousCompressedSparseMatrix rep) }

deriving instance Show (ContiguousCompressedSparseMatrix rep)
    => Show (Format CompressedSparseRow 'Contiguous ('S ('S 'Z)) rep)

newtype instance Format CompressedSparseColumn 'Contiguous ('S ('S 'Z)) rep =
    FormatContiguousCompressedSparseColumn {
      _getFormatContiguousCSC :: (ContiguousCompressedSparseMatrix rep) }

deriving instance Show (ContiguousCompressedSparseMatrix rep)
    => Show (Format CompressedSparseColumn 'Contiguous ('S ('S 'Z)) rep)

newtype instance Format CompressedSparseRow 'InnerContiguous ('S ('S 'Z)) rep =
    FormatInnerContiguousCompressedSparseRow {
      _getFormatInnerContiguousCSR :: (InnerContiguousCompressedSparseMatrix rep )
  }
deriving instance  Show (InnerContiguousCompressedSparseMatrix rep )
    =>  Show (Format CompressedSparseRow 'InnerContiguous ('S ('S 'Z)) rep)

newtype instance Format CompressedSparseColumn 'InnerContiguous ('S ('S 'Z)) rep =
    FormatInnerContiguousCompressedSparseColumn {
      _getFormatInnerContiguousCSC :: (InnerContiguousCompressedSparseMatrix rep )
  }

deriving instance Show (InnerContiguousCompressedSparseMatrix rep )
  =>  Show (Format CompressedSparseColumn 'InnerContiguous ('S ('S 'Z)) rep)

      --deriving (Show,Eq,Data)

{-
  FormatInnerContiguous rowsize columnsize

-}
--newtype instance Format CompressedSparseColumn Contiguous (S (S Z)) rep =
--    FormatContiguousCompressedSparseColumn {
--      _getFormatContiguousCSC ::  (ContiguousCompressedSparseMatrix rep)
--  }
    --deriving (Show,Eq,Data)

--newtype  instance Format CompressedSparseColumn InnerContiguous (S (S Z)) rep =
--    FormatInnerContiguousCompressedSparseColumn {
--     _getFormatInnerContiguousCSC :: (InnerContiguousCompressedSparseMatrix rep)
--  }
--    --deriving (Show,Eq,Data)

--CSR and CSC go here, and their version of lookups and next address and next index






--  Offset binary search --- cribbed with permission from
-- edward kmett's structured lib

{-
todo: theres some neat micro optimizations that are
possible If I know how indexed structures are paged aligned and what not
eg, when binary search, check both the first and last slot of a page I land on.
Also on >= Nehalem, pages are "paired" so if you land on the lower page, the
upper page is always loaded, etc etc. Not doing these for now.
-}




{-
-- Assuming @l <= h@. Returns @h@ if the predicate is never @True@ over @[l..h)@
-- requires p be a "monotonic" predicate  (FFFFFTTTTT)
-}
bsearchUp :: (Int -> Bool) -> Int -> Int -> Int
bsearchUp p = go where
  go l h
    | l == h    = l
    | p m       = go l m
    | otherwise = go (m+1) h
    where hml = h - l
          m = l + unsafeShiftR hml 1 + unsafeShiftR hml 6
{-# INLINE bsearchUp #-}
{-
 Assuming @l <= h@. Returns @l@ if the predicate is never @True@ over @(l..h]@
  assumes predicate p is monotonic decreasing TTTTTFFFFF
  -}
bsearchDown :: (Int -> Bool) -> Int -> Int -> Int
bsearchDown p = go where
  go l h
    | l == h    = l
    | p (m+1)       = go (m+1) h
    | otherwise = go l m
    where hml = h - l
          m = l + unsafeShiftR hml 1 + unsafeShiftR hml 6
{-# INLINE bsearchDown #-}

{-
-- Assuming @l <= h@. Returns @h@ if the predicate is never @True@ over @[l..h)@
-- requires p be a "monotonic" predicate  (FFFFFTTTTT)
-- does a linear scan on the first constant number of slots
(for now 97 because i had to pick a number thats ~ log MaxInt)
and then falls back to binary search.
Meant to have O(1) average case, O(log n) worst case
-}
basicHybridSearchUp :: (Int -> Bool ) -> Int -> Int -> Int
basicHybridSearchUp  p = goCaseMe where
  goCaseMe l h  | (h-l <= magicConstant) || p magicConstant
                  {- either the range is short, OR
                    we know match happens in the first magicConstant size subrange
                  -}
                    = linearSearchUp p l (min h magicConstant)
                | otherwise = bsearchUp p magicConstant h
{-# INLINE  basicHybridSearchUp #-}


basicHybridSearchDown :: (Int -> Bool)-> Int -> Int -> Int
basicHybridSearchDown  p = goCaseMe where
  goCaseMe l h  | (h-l <= magicConstant)  || p (h- magicConstant)
                {-  either the range is short, OR
                    we know match happens in the first magicConstant size subrange
                 -}
                    = linearSearchDown p  (max l (h - magicConstant)) h
                | otherwise = bsearchDown p l (h - magicConstant)
{-# INLINE basicHybridSearchDown #-}

{-
i chose 97 because it seemed like a number thats ~ log MaxInt always (within 4x)
And is a range that should stay in L1 cache sizes for most purposes
-}
magicConstant :: Int
magicConstant = 97


-- Assuming @l <= h@. Returns @h@ if the predicate is never @True@ over @[l..h)@
linearSearchUp :: (Int -> Bool)-> Int -> Int -> Int
linearSearchUp p = go where
  go l h
    | l ==h = l
    | p l = l
    | otherwise = go (l+1) h
{-#INLINE linearSearchUp #-}

-- Assuming @l <= h@. Returns @l@ if the predicate is never @True@ over @(l..h]@
linearSearchDown :: (Int -> Bool)-> Int -> Int -> Int
linearSearchDown p = go where
  go l h
    | l ==h = l
    | p h = h
    | otherwise = go l (h-1)
{-#INLINE linearSearchDown #-}




--
-- now assumed each key is unique and ordered
--
-- Assuming @l <= h@. Returns @h@ if the predicate is never @True@ over @[l..h)@

-- should at some point try out a ternary search scheme to have even better
-- cache behavior (and benchmark of course)

searchOrd :: (Int -> Ordering) -> Int -> Int -> Int
searchOrd  p = go where
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
{-# INLINE searchOrd #-}

-- | Returns index of 'key' if 'key' is in 'ks'.
lookupExact :: (Ord k, V.Vector vec k) => vec k -> k -> Maybe Int
lookupExact ks key
  | j <- searchOrd (\i -> compare (ks V.! i)  key) 0 (V.length ks - 1)
  , ks V.! j == key = Just $! j
  | otherwise = Nothing
{-# INLINE lookupExact #-}

lookupExactRange :: (Ord k, V.Vector vec k) => vec k -> k -> Int -> Int -> Maybe Int
lookupExactRange  ks key lo hi
  | j <- searchOrd (\i -> compare (ks V.! i)  key) lo hi
  , ks V.! j == key = Just $! j
  | otherwise = Nothing
{-# INLINE lookupExactRange  #-}

--lookupLUB ::  (Ord k, V.Vector vec k) => vec k -> k -> Maybe Int
--lookupLUB  ks key
--  | j <- search  (\i -> compare (ks V.! i)  key) 0 (V.length ks - 1)
--  , ks V.! j <= key = Just $! j
--  | otherwise = Nothing
--{-# INLINE lookupLUB  #-}

type instance  Transposed (Format DirectSparse 'Contiguous ('S 'Z) rep )=
   (Format DirectSparse 'Contiguous ('S 'Z) rep )




type instance LayoutAddress (Format DirectSparse 'Contiguous ('S 'Z) rep) =  Address


instance V.Vector (BufferPure rep) Int
  => Layout  (Format DirectSparse 'Contiguous ('S 'Z) rep ) ('S 'Z) where

  transposedLayout  = id
  -- {-# INLINE transposedLayout #-}

  basicLogicalShape = \ form -> _dsLogicalShape form  :* Nil
  -- {-# INLINE basicLogicalShape #-}

  basicCompareIndex = \ _ (a:* Nil) (b :* Nil) ->compare a b
  -- {-# INLINE basicCompareIndex #-}

  basicAddressRange form = Range <$> minAddress form <*> maxAddress form
    where
        minAddress =
          \ (FormatDirectSparseContiguous _ _   lookupTable)->
              if  V.length lookupTable >0 then  Just $! Address 0 else Nothing

        maxAddress =
          \ (FormatDirectSparseContiguous _ _   lookupTable)->
            if (V.length lookupTable >0 )
               then Just $! Address (V.length lookupTable - 1 )
               else Nothing

-- TODO, double check that im doing shift correctly
  {-# INLINE indexToAddress #-}
  indexToAddress =
      \ (FormatDirectSparseContiguous shape  indexshift lookupTable) (ix:*_)->
         if  not (ix < shape && ix > 0 ) then  Nothing
          else  fmap Address  $! lookupExact lookupTable (ix + indexshift)

  {-# INLINE addressToIndex #-}
  addressToIndex =
    \ (FormatDirectSparseContiguous _ shift lut) (Address addr) ->
        ((lut V.! addr ) - shift) :* Nil
  {-# INLINE basicAddressAsInt #-}
  basicAddressAsInt = \ _ (Address a) -> a

  {-# INLINE basicNextAddress #-}
  basicNextAddress =
    \ (FormatDirectSparseContiguous _ _ lut) (Address addr) ->
      if  addr >= (V.length lut) then Nothing else Just  (Address (addr+1))

  -- {-# INLINE basicAddressPopCount #-}
  basicAddressPopCount = \ form (Range loadr@(Address lo) hiadr@(Address hi)) ->
    if not ( lo <= hi ) then
      error $! "basicAddressPopCount was passed a bad Address Range " ++ show loadr ++" " ++ show hiadr
      else
        case  basicAddressRange form of
          Nothing -> 0
          Just (Range (Address loBound) (Address  hiBound)) ->
            if not $ (loBound<= lo ) && (hi <= hiBound)
              then error $!
               "basicAddressPopCount was passed a bad Address Range: "
                ++show lo++" "++ show hi++"\nwith format Address range"
                ++ show loBound ++ " " ++ show hiBound
              else hi - lo


{-
    i've said it before, i'll say it again, scanning forward in the index space
    for sparse structures is really weird, :)

    NOTE: also need to remember to do those index space shifts for
    1dim direct sparse, and test them thoroughly
-}
  -- {-# INLINE basicNextIndex #-}
  basicNextIndex =
    \form@(FormatDirectSparseContiguous size shift lut) (ix:*Nil) mebeAddress ->
      if  ix >= size || ix >= (lut V.! (V.length lut -1) - shift ) then Nothing
            -- if ix is out of bounds or the last element, we're done!
      else
        let
            resAddr = Address $! bsearchUp  (\lix-> ix < ((lut V.! lix)-shift) )
                        0 (V.length lut )
        in case mebeAddress of
          Nothing ->  resAddr `seq` (Just (addressToIndex form resAddr ,  resAddr))
                -- Q: do i want the Index part of the tuple to be strict or not?
                -- leaving it lazy for now
                -- TODO / FIX / AUDIT ME / NOT SURE
              -- this is the fall back binary search based lookup

          Just (Address adr)->
          -- make sure the address hint is in bounds and
          -- is <= the current position
              if adr >0 && adr < (V.length lut -1) && ix >=((lut V.! adr )-shift)
              then
                -- by construction we know theres at least one applicable index
                -- thats
                let !nextAddr = Address $!
                                basicHybridSearchUp
                                  (\lix-> ix <  ((lut V.! lix)-shift ) )
                                  adr (V.length lut -1)
                  in  Just (addressToIndex form nextAddr ,  nextAddr)
              else
                resAddr `seq` (Just (addressToIndex form resAddr ,  resAddr))


------------
------------

type instance Transposed (Format CompressedSparseRow 'Contiguous ('S ('S 'Z)) rep )=
    (Format CompressedSparseColumn 'Contiguous ('S ('S 'Z)) rep )


type instance LayoutAddress (Format CompressedSparseRow 'Contiguous ('S ('S 'Z)) rep ) = SparseAddress

instance  (V.Vector (BufferPure rep) Int )
  => Layout  (Format CompressedSparseRow 'Contiguous ('S ('S 'Z)) rep ) ('S ('S 'Z)) where

  transposedLayout  = \(FormatContiguousCompressedSparseRow repFormat) ->
                          (FormatContiguousCompressedSparseColumn  repFormat)
  {-# INLINE transposedLayout #-}


  basicLogicalShape = \ form ->  (_ccsmInnerDim $ _getFormatContiguousCSR  form ) :*
         ( _ccsmOuterDim $ _getFormatContiguousCSR form ):* Nil
          --   x_ix :* y_ix
  {-# INLINE basicLogicalShape #-}


  basicCompareIndex = \ _ as  bs ->shapeCompareRightToLeft as bs
  {-# INLINE basicCompareIndex #-}


  {-# INLINE basicAddressPopCount #-}
  basicAddressPopCount = \ form (Range (SparseAddress _ lo) (SparseAddress _ hi)) ->
    if not ( lo <= hi ) then
      error $! "basicAddressPopCount was passed a bad Address Range " ++ show lo ++" " ++ show hi
      else
        case  basicAddressRange form of
          Nothing -> 0
          Just (Range (SparseAddress _ loBound) (SparseAddress _ hiBound)) ->
            if not $ (loBound<= lo ) && (hi <= hiBound)
              then error $!
               "basicAddressPopCount was passed a bad SparseAddress Range: "
                ++show lo++" "++ show hi++"\nwith format SparseAddress range"
                ++ show loBound ++ " " ++ show hiBound
              else hi - lo

   -- {-# INLINE rangedFormatAddress #-}
  basicAddressRange = \ form ->
    case (minAddress form,maxAddress form) of
      (Just least, Just greatest)-> Just (Range least greatest)
      _ -> Nothing

    where
      {-
      probably should deduplicate min/maxAddress
      -}
      minAddress =
            \(FormatContiguousCompressedSparseRow
                (FormatContiguousCompressedSparseInternal  y_row_range x_col_range
                                                            columnIndex rowStartIndex)) ->
                    if  y_row_range < 1  || x_col_range < 1|| (V.length columnIndex  < 1)
                      then Nothing
                      else
                      -- the value buffer has the invariant the the end points
                      -- of the buffer MUST be valid  in bounds values if length buffer > 0
                    --SparseAddress $! 0 $! 0

                    -- hoisted where into if branch as let so lets could be strict
                        let
                          !addrShift = columnIndex V.! 0

                          -- for now assuming candidateRow is ALWAYS valid
                          --- haven't proven this, FIXME
                          !candidateRow= {-linearSearchUp-}
                               basicHybridSearchUp nonZeroRow 0 (y_row_range-1 )


                          {- FIXME, to get the right complexity
                          to linear search on first log #rows + 1 slots, then fall
                          back to binary search
                          punting for now because this probably wont matter than often

                          the solution will be to replace linearSearchUp
                          with a hybridSearchUp
                           -}
                          nonZeroRow =
                              \ !row_ix ->
                                   -- the first row to satisfy this property
                                  (rowStartIndex V.! (row_ix+1) >  rowStartIndex V.! row_ix)
                                  -- if the start index is >0, already past the min address row!
                                    ||  (rowStartIndex V.! row_ix) - addrShift > 0

                                  --else  maxIxP1 >  rowStartIndex V.! row_ix
                        in Just $! SparseAddress  candidateRow $! 0

      maxAddress  =
        \(FormatContiguousCompressedSparseRow
            (FormatContiguousCompressedSparseInternal   y_row_range x_col_range
                                                        columnIndex rowStartIndex)) ->
                if  y_row_range < 1  || x_col_range < 1|| (V.length columnIndex  < 1)
                  then Nothing
                  else
                  -- the value buffer has the invariant the the end points
                  -- of the buffer MUST be valid  in bounds values if length buffer > 0
                --SparseAddress $! 0 $! 0

                -- hoisted where into if branch as let so lets could be strict
                    let
                      !addrShift = columnIndex V.! 0
                      !maxIxP1 = V.length columnIndex

                      -- for now assuming candidateRow is ALWAYS valid
                      --- haven't proven this, FIXME
                      !candidateRow= {-linearSearchDown-}
                          basicHybridSearchDown nonZeroRow 0 (y_row_range-1 )


                      {- FIXME, to get the right complexity
                      to linear search on last log #rows + 1 slots, then fall
                      back to binary search
                      punting for now because this probably wont matter than often

                      the solution will be to replace linearSearchDown
                      with a hybridSearchDown
                       -}
                      nonZeroRow =
                          \ !row_ix ->
                       -- the first row to satisfy this property (going down from last row)
                              (rowStartIndex V.! (row_ix+1) >  rowStartIndex V.! row_ix)
                      -- if the start index is >= maxIxP1, havent gone down to max addres yet
                      -- if < maxIxp1, we're at or below the max address
                                ||  (rowStartIndex V.! row_ix) - addrShift < maxIxP1

                              --else  maxIxP1 >  rowStartIndex V.! row_ix
                    in
                        Just $!
                         SparseAddress  candidateRow $! (V.length columnIndex - 1 )

       -- \ (FormatContiguousCompressedSparseRow
       -- (FormatContiguousCompressedSparseInternal _ y_range
       --          columnIndex _)) ->
       --       SparseAddress (y_range - 1) (V.length columnIndex - 1 )

  {-#  INLINE basicAddressAsInt #-}
  basicAddressAsInt = \ _ (SparseAddress _ addr)-> addr

  {-# INLINE addressToIndex #-}
  addressToIndex =
        \ (FormatContiguousCompressedSparseRow
            (FormatContiguousCompressedSparseInternal  _ _ columnIndex _))
            (SparseAddress outer inner) ->
              (columnIndex V.! inner ) :* outer :*  Nil
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
         \ (FormatContiguousCompressedSparseRow
            (FormatContiguousCompressedSparseInternal  _ _
              columnIndex rowStartIndex))
            (SparseAddress outer inner) ->
              if  inner < (V.length columnIndex -1)
               -- can advance further
                 -- && ( outer == (y_row_range-1)
                  --- either last row
                  || ((inner +1) < (rowStartIndex V.! (outer + 1)  - (rowStartIndex V.! 0 )))
                     -- or our address is before the next row starts
                     -- 3 vector CSR has a +1 slot at the end of the rowStartIndex

                then
                  Just (SparseAddress outer (inner+1))
                else
                  if inner == (V.length columnIndex -1)
                    then Nothing
                    else Just (SparseAddress (outer + 1) (inner + 1 ) )


  -- {-# INLINE indexToAddress #-}
  indexToAddress =
        \ (FormatContiguousCompressedSparseRow
            (FormatContiguousCompressedSparseInternal  y_row_range x_col_range
              columnIndex rowStartIndex))
          (ix_x:*ix_y :* _ ) ->
            if  not (ix_x >= x_col_range ||  ix_y >=y_row_range )
              then
              -- slightly different logic when ix_y < range_y-1 vs == range_y-1
              -- because contiguous, don't need the index space shift though!
                let
                  shift = (rowStartIndex V.! 0)
                  checkIndex i =
                      if  (columnIndex V.!i) == ix_x
                        then Just i
                        else Nothing
                in
                 (SparseAddress ix_y  <$>) $!
                    checkIndex =<<
                 --- FIXME  : need to check
                      lookupExactRange columnIndex ix_x
                          ((rowStartIndex V.! ix_y) - shift)
                          ((rowStartIndex V.! (ix_y+1) ) - shift)

              else   (Nothing :: Maybe SparseAddress )


  -- {-# INLINE basicNextIndex #-}
  {-  because nextIndex acts like a range query
      it doesn't make sense for inner loops
  -}
  basicNextIndex =
     \_form@(FormatContiguousCompressedSparseRow
              (FormatContiguousCompressedSparseInternal
                y_row_range x_col_range _columnIndex _rowStartIndex))
      _ix@(innerX :* outerY :*Nil) mebeSparseAddress ->
        if  not $ (innerX >=0 && innerX  < x_col_range ) && (outerY >= 0 && outerY < y_row_range)
          -- checking if index is inbounds for logical shape
          -- return Nothing if its out of bounds
          -- QUESTION: should it throw an error instead of returning nothing?
        then Nothing
        else
          case mebeSparseAddress of
            Nothing -> error "finish me "
              where
              {- Okay here we check if the proposed current index is manifest, or not
                Is it the right Row To search for the next index,
                Or if We need to search further along. This is the way that
                enables Usage of operations That give a complexity that is O(1)
                in the average/best case and O(log N )in the worst case

                The logical we do is roughly first check If there is an element
                strictly Greater than ix in next we are doing the successor
                That is within that Row And if so we can directly
                  do a binary search therein
                -}
                _resRow = error "finish me "

            (Just (SparseAddress _innerix _outerix) )
                -> error "really finish me"


        --case mebeAddress of
        --  Nothing ->
        --    let
        --    resAddr = Address $! bsearchUp  (\lix-> ix < ((lut V.! lix)-shift) )
        --                0 (V.length lut )
        --  in
        --   resAddr `seq` (Just (basicToIndex form resAddr ,  resAddr))
        --        -- Q: do i want the Index part of the tuple to be strict or not?
        --        -- leaving it lazy for now
        --        -- TODO / FIX / AUDIT ME / NOT SURE
        --      -- this is the fall back binary search based lookup
        --  Just (Address adr)->
        --  -- make sure the address hint is in bounds and
        --  -- is <= the current position
        --      if adr >0 && adr < (V.length lut -1) && ix >=((lut V.! adr )-shift)
        --      then
        --        -- by construction we know theres at least one applicable index
        --        -- thats
        --        let !nextAddr = Address $!
        --                        basicHybridSearchUp
        --                          (\lix-> ix <  ((lut V.! lix)-shift ) )
        --                          adr (V.length lut -1)
        --          in  Just (basicToIndex form nextAddr ,  nextAddr)
        --      else
        --        resAddr `seq` (Just (basicToIndex form resAddr ,  resAddr))


-------
-------



--type instance Transposed (Format CompressedSparseRow InnerContiguous (S (S Z)) rep )=
--    (Format CompressedSparseColumn InnerContiguous (S (S Z)) rep )

--type instance Transposed (Format CompressedSparseColumn InnerContiguous (S (S Z)) rep )=
--    (Format CompressedSparseRow InnerContiguous (S (S Z)) rep )


--instance Layout (Format CompressedSparseRow InnerContiguous (S (S Z)) rep ) (S (S Z)) where
--  transposedLayout  = \(FormatInnerContiguousCompressedSparseRow a b c d e f) ->
--    (FormatInnerContiguousCompressedSparseColumn a b c d e f)
--  {-# INLINE transposedLayout #-}
--  basicFormShape = \ form -> logicalRowShapeInnerContiguousCSR form  :*
--         logicalColumnShapeInnerContiguousCSR form :* Nil
--  {-# INLINE basicFormShape #-}
--  basicCompareIndex = \ _ as  bs ->shapeCompareRightToLeft as bs
--  {-# INLINE basicCompareIndex#-}



--instance  (V.Vector (BufferPure rep) Int )
--  => SparseLayout (Format CompressedSparseRow InnerContiguous (S (S Z)) rep ) (S (S Z)) where

--      type LayoutAddress (Format CompressedSparseRow
--          InnerContiguous (S (S Z)) rep ) = SparseAddress

--      {-# INLINE minSparseAddress #-}
--      minSparseAddress = \_ -> SparseAddress 0 0

--      {-# INLINE maxSparseAddress#-}
--      maxSparseAddress  =
--       \ (FormatInnerContiguousCompressedSparseInternal _ outer_dim_range _
--          innerDimIndex _) ->
--              SparseAddress (outer_dim_range - 1) (V.length innerDimIndex - 1 )


--      {-#INLINE basicToIndex #-}
--      basicToIndex =
--       \ (FormatInnerContiguousCompressedSparseInternal _ _  _ innerDimIndex _)
--          (SparseAddress outer inner) -> (innerDimIndex V.! inner ) :* outer :*  Nil
--          -- outer is the row (y index) and inner is the lookup position for the x index



--theres 3 cases for contiguous next address:
--in the middle of a run on a fixed outer dimension,
--need to bump the outer dimension, or we're at the end of the entire array

--we make the VERY strong assumption that no illegal addresses are ever made!

--note that for very very small sparse matrices, the branching will have some
--overhead, but in general branch prediction should work out ok.

--      {-# INLINE basicNextAddress #-}
--      basicNextAddress =
--         \ (FormatInnerContiguousCompressedSparseRow
--                (FormatInnerContiguousCompressedSparseInternal _ _ _
--                                                         columnIndex rowstartIndex))
--            (SparseAddress outer inner) ->
--              if not  (inner == (V.length columnIndex -1)
--                                          {- && outer == (y_range-1) -}
--                     || (inner +1) == (rowstartIndex V.! (outer + 1)))
--                then
--                  Just (SparseAddress outer (inner+1))
--                else
--                  if inner == (V.length columnIndex -1)
--                    then Nothing
--                    else Just (SparseAddress (outer + 1) (inner + 1 ) )

--        --  error "finish me damn it"
--      {-# INLINE basicToSparseAddress #-}
--      basicToSparseAddress =
--        \ (FormatInnerContiguousCompressedSparseRow
--            (FormatInnerContiguousCompressedSparseInternal x_range y_range addrShift
--                      columnIndex rowstartIndex))
--          (ix_x:*ix_y :* _ ) ->
--            if  not (ix_x >= x_range ||  ix_y >=y_range )
--              then
--              -- slightly different logic when ix_y < range_y-1 vs == range_y-1
--              -- because contiguous, don't need the index space shift though!
--                       SparseAddress ix_y   <$>
--                          lookupExactRange columnIndex ix_x
--                              -- ((rowstartIndex V.! ix_y) - addrShift)
--                            (if  ix_y < (y_range-1)
--                              -- addr shift is for correcting from a major axis slice
--                              then  (rowstartIndex V.! (ix_y+1) ) - addrShift
--                              else V.length columnIndex  - 1 )
--              else   (Nothing :: Maybe SparseAddress )
