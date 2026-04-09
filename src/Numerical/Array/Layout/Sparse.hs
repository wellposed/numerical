{-
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
import Data.Bits (unsafeShiftR, shiftR, shiftL, (.&.))
import Control.Applicative
import Numerical.Array.Layout.Base
--import Numerical.Array.Shape
import Numerical.InternalUtils
import qualified  Data.Vector.Generic as V
import Prelude hiding (error )


-- | Skip-encoded rowptr for CSR/CSC: O(1) non-empty row lookup.
--
-- Normal rowptr entries are non-negative buffer offsets.
-- Empty rows get a negative entry encoding forward and backward skip distances
-- to the nearest non-empty rows. This turns the O(rows) empty-row scan
-- in 'seek' into O(1).
--
-- Encoding (for a negative entry):
--   magnitude = abs(entry)
--   fwd_skip  = magnitude `shiftR` 31    -- upper 32 bits: rows forward to next non-empty
--   bwd_skip  = magnitude .&. 0x7FFFFFFF -- lower 31 bits: rows backward to prev non-empty
--
-- Convention: skip of 0 means "no non-empty row in that direction."

-- | Encode an empty row's skip metadata as a negative Int.
encodeEmptyRow :: Int -> Int -> Int
encodeEmptyRow fwdSkip bwdSkip =
  negate ((fwdSkip `shiftL` 31) + (bwdSkip .&. 0x7FFFFFFF))
{-# INLINE encodeEmptyRow #-}

-- | Is this rowptr entry an empty row (negative)?
isEmptyRowEntry :: Int -> Bool
isEmptyRowEntry x = x < 0
{-# INLINE isEmptyRowEntry #-}

-- | Extract forward skip from a negative rowptr entry.
-- Caller must ensure the entry is negative.
fwdSkip :: Int -> Int
fwdSkip x = abs x `shiftR` 31
{-# INLINE fwdSkip #-}

-- | Extract backward skip from a negative rowptr entry.
-- Caller must ensure the entry is negative.
bwdSkip :: Int -> Int
bwdSkip x = abs x .&. 0x7FFFFFFF
{-# INLINE bwdSkip #-}

-- | Given a skip-encoded rowptr and a row index, resolve the actual buffer offset.
-- For non-empty rows: the entry itself.
-- For empty rows: follow the forward skip to the next non-empty row
-- and use its offset (which equals the end of the last non-empty row before it).
-- If no non-empty row exists forward, uses the sentinel (last entry, always non-negative).
resolveRowStart :: V.Vector vec Int => vec Int -> Int -> Int
resolveRowStart rowptr i =
  let !entry = rowptr V.! i
  in if entry >= 0
     then entry
     else let !skip = fwdSkip entry
          in if skip > 0
             then rowptr V.! (i + skip)  -- next non-empty row's start = our start
             else rowptr V.! (V.length rowptr - 1)  -- sentinel: total nnz
{-# INLINE resolveRowStart #-}

-- | Build a skip-encoded rowptr from a traditional (monotone non-negative) rowptr.
-- The input must have length nrows+1 with the last entry being nnz.
-- Empty rows (where rowptr[i] == rowptr[i+1]) get skip-encoded.
buildSkipRowPtr :: (V.Vector vec Int, V.Vector vec Int) => vec Int -> vec Int
buildSkipRowPtr traditional =
  let !n = V.length traditional - 1  -- number of rows
      isEmpty i = (traditional V.! i) == (traditional V.! (i+1))

      -- Forward pass: compute fwd_skip for each empty row
      -- fwd_skip[i] = distance to next non-empty row (or 0 if none)
      fwdSkips = V.generate n $ \i ->
        if not (isEmpty i) then 0
        else let go j | j >= n = 0          -- no non-empty row after us
                      | not (isEmpty j) = j - i
                      | otherwise = go (j+1)
             in go (i+1)

      -- Backward pass: compute bwd_skip for each empty row
      bwdSkips = V.generate n $ \i ->
        if not (isEmpty i) then 0
        else let go j | j < 0 = 0           -- no non-empty row before us
                      | not (isEmpty j) = i - j
                      | otherwise = go (j-1)
             in go (i-1)

      -- Build the encoded rowptr (n+1 entries, last is sentinel = nnz)
  in V.generate (n+1) $ \i ->
       if i == n then traditional V.! n  -- sentinel always non-negative
       else if isEmpty i
            then encodeEmptyRow (fwdSkips V.! i) (bwdSkips V.! i)
            else traditional V.! i
{-# INLINABLE buildSkipRowPtr #-}


data CompressedSparseRow
  deriving (Typeable)

type CSR = CompressedSparseRow

data CompressedSparseColumn
    deriving (Typeable)

type CSC = CompressedSparseColumn

data DirectSparse
    deriving (Typeable)



data instance Format DirectSparse 'Contiguous ('S 'Z) rep =
    FormatDirectSparseContiguous {
      _logicalShapeDirectSparse:: {-# UNPACK#-} !Int
      ,_logicalBaseIndexShiftDirectSparse::{-# UNPACK#-} !Int
      ,_indexTableDirectSparse :: !(BufferPure rep Int )  }


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

data ContiguousCompressedSparseMatrix rep =
    FormatContiguousCompressedSparseInternal {
     -- does this need the index space shift for outer range slices???
      _outerDimContiguousSparseFormat ::  {-# UNPACK #-} !Int
      ,_innerDimContiguousSparseFormat ::  {-# UNPACK #-} !Int
      ,_innerDimIndexContiguousSparseFormat :: !(BufferPure rep Int)
      ,_outerDim2InnerDimContiguousSparseFormat:: !(BufferPure rep Int )
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
   FormatInnerContiguousCompressedSparseInternal {
      _outerDimInnerContiguousSparseFormat ::    {-# UNPACK #-} !Int
      ,_innerDimInnerContiguousSparseFormat ::  {-# UNPACK #-} !Int
      ,_innerDimIndexShiftInnerContiguousSparseFormat:: {-# UNPACK #-} !Int

      ,_innerDimIndexInnerContiguousSparseFormat :: !(BufferPure rep Int)
      ,_outerDim2InnerDimStartInnerContiguousSparseFormat:: !(BufferPure rep Int )
      ,_outerDim2InnerDimEndInnerContiguousSparseFormat:: !(BufferPure rep Int )
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


also should compare against search strategies defined in
the vector-algorithms package, namely the
galloping ones
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

type instance LayoutLogicalFormat (Format DirectSparse 'Contiguous ('S 'Z) rep )
    = (Format DirectSparse 'Contiguous ('S 'Z) rep )
instance V.Vector (BufferPure rep) Int => Layout  (Format DirectSparse 'Contiguous ('S 'Z) rep ) ('S 'Z) where

  transposedLayout  = id
  -- {-# INLINE transposedLayout #-}

  logicalShape = \ form -> _logicalShapeDirectSparse form  :* Nil
  -- {-# INLINE logicalShape #-}

  logicalForm = id

  compareIndex = \ _ (a:* Nil) (b :* Nil) -> compare a b
  -- {-# INLINE compareIndex #-}

  addressRange = \form ->
    case (minAddress form , maxAddress form ) of
      (Just least, Just greatest) -> Just (Range least greatest )
      _ -> Nothing

    where
        minAddress =
          \ (FormatDirectSparseContiguous _ _   lookupTable) ->
              if  V.length lookupTable >0 then  Just $! Address 0 else Nothing

        maxAddress =
          \ (FormatDirectSparseContiguous _ _   lookupTable) ->
            if (V.length lookupTable >0 )
               then Just $! Address (V.length lookupTable - 1 )
               else Nothing

-- TODO, double check that im doing shift correctly
  {-# INLINE toAddress #-}
  toAddress =
      \ (FormatDirectSparseContiguous shape  indexshift lookupTable) (ix:*_) ->
         if  not (ix < shape && ix > 0 ) then  Nothing
          else  fmap Address  $! lookupExact lookupTable (ix + indexshift)

  {-# INLINE toIndex #-}
  toIndex =
    \ (FormatDirectSparseContiguous _ shift lut) (Address addr) ->
        ((lut V.! addr ) - shift) :* Nil
  {-# INLINE addressAsInt #-}
  addressAsInt = \ _ (Address a) -> a

  {-# INLINE nextAddr #-}
  nextAddr =
    \ (FormatDirectSparseContiguous _ _ lut) (Address addr) ->
      if  addr >= (V.length lut) then Nothing else Just  (Address (addr+1))

  -- {-# INLINE addressPopCount #-}
  addressPopCount = \ form (Range loadr@(Address lo) hiadr@(Address hi)) ->
    if not ( lo <= hi ) then
      error $! "addressPopCount was passed a bad Address Range " ++ show loadr ++" " ++ show hiadr
      else
        case  addressRange form of
          Nothing -> 0
          Just (Range (Address loBound) (Address  hiBound)) ->
            if not $ (loBound<= lo ) && (hi <= hiBound)
              then error $!
               "addressPopCount was passed a bad Address Range: "
                ++show lo++" "++ show hi++"\nwith format Address range"
                ++ show loBound ++ " " ++ show hiBound
              else hi - lo


{-
    i've said it before, i'll say it again, scanning forward in the index space
    for sparse structures is really weird, :)

    NOTE: also need to remember to do those index space shifts for
    1dim direct sparse, and test them thoroughly
-}
  -- {-# INLINE seek #-}
  seek =
    \form@(FormatDirectSparseContiguous size shift lut) (ix:*Nil) mebeAddress ->
      if  ix >= size || ix >= (lut V.! (V.length lut -1) - shift ) then Nothing
            -- if ix is out of bounds or the last element, we're done!
      else
        let
            resAddr = Address $! bsearchUp  (\lix-> ix < ((lut V.! lix)-shift) )
                        0 (V.length lut )
        in case mebeAddress of
          Nothing ->  resAddr `seq` (Just (toIndex form resAddr ,  resAddr))
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
                  in  Just (toIndex form nextAddr ,  nextAddr)
              else
                resAddr `seq` (Just (toIndex form resAddr ,  resAddr))

  -- Sparse rank-1 affine shift: walk via nextAddr forward or prevAddr backward. O(|step|).
  affineAddressShift = \form addr step ->
    if step == 0 then Just addr
    else if step > 0
      then let go 0 a = Just a
               go n a = case nextAddr form a of
                          Nothing -> Nothing
                          Just a' -> go (n-1) a'
           in go step addr
      else -- negative: for rank-1 sparse, addresses are plain Ints, just decrement
           let (Address pos) = addr
               newPos = pos + step  -- step is negative
           in if newPos >= 0 then Just (Address newPos) else Nothing


------------
------------

type instance Transposed (Format CompressedSparseRow 'Contiguous ('S ('S 'Z)) rep )=
    (Format CompressedSparseColumn 'Contiguous ('S ('S 'Z)) rep )


type instance LayoutAddress (Format CompressedSparseRow 'Contiguous ('S ('S 'Z)) rep ) = SparseAddress
type instance LayoutLogicalFormat (Format CompressedSparseRow 'Contiguous ('S ('S 'Z)) rep ) = (Format CompressedSparseRow 'Contiguous ('S ('S 'Z)) rep )

instance  (V.Vector (BufferPure rep) Int )
  => Layout  (Format CompressedSparseRow 'Contiguous ('S ('S 'Z)) rep ) ('S ('S 'Z)) where

  transposedLayout  = \(FormatContiguousCompressedSparseRow repFormat) ->
                          (FormatContiguousCompressedSparseColumn  repFormat)
  {-# INLINE transposedLayout #-}


  logicalShape = \ form -> (_innerDimContiguousSparseFormat $ _getFormatContiguousCSR  form ) :*
         ( _outerDimContiguousSparseFormat $ _getFormatContiguousCSR form ):* Nil
          --   x_ix :* y_ix
  {-# INLINE logicalShape #-}


  compareIndex = \ _ as  bs -> shapeCompareRightToLeft as bs
  {-# INLINE compareIndex #-}


  {-# INLINE addressPopCount #-}
  addressPopCount = \ form (Range (SparseAddress _ lo) (SparseAddress _ hi)) ->
    if not ( lo <= hi ) then
      error $! "addressPopCount was passed a bad Address Range " ++ show lo ++" " ++ show hi
      else
        case  addressRange form of
          Nothing -> 0
          Just (Range (SparseAddress _ loBound) (SparseAddress _ hiBound)) ->
            if not $ (loBound<= lo ) && (hi <= hiBound)
              then error $!
               "addressPopCount was passed a bad SparseAddress Range: "
                ++show lo++" "++ show hi++"\nwith format SparseAddress range"
                ++ show loBound ++ " " ++ show hiBound
              else hi - lo

   -- {-# INLINE rangedFormatAddress #-}
  addressRange = \ form ->
    case (minAddress form,maxAddress form) of
      (Just least, Just greatest)-> Just (Range least greatest)
      _ -> Nothing

    where
      minAddress =
            \(FormatContiguousCompressedSparseRow
                (FormatContiguousCompressedSparseInternal  y_row_range x_col_range
                                                            columnIndex rowStartIndex)) ->
                    if  y_row_range < 1  || x_col_range < 1|| (V.length columnIndex  < 1)
                      then Nothing
                      else
                        let
                          !shift = rowStartIndex V.! 0

                          -- With skip encoding: if row 0 entry is negative, use fwdSkip.
                          -- Otherwise: hybrid search for first non-empty row.
                          !candidateRow =
                            let !entry0 = rowStartIndex V.! 0
                            in if isEmptyRowEntry entry0
                               then let !skip = fwdSkip entry0
                                    in if skip > 0 then skip else y_row_range -- no non-empty rows
                               else if resolveRowStart rowStartIndex 1 - shift > 0
                                    then 0  -- row 0 is non-empty
                                    else basicHybridSearchUp
                                           (\r -> not (isEmptyRowEntry (rowStartIndex V.! r))
                                               && resolveRowStart rowStartIndex (r+1) - shift
                                                > resolveRowStart rowStartIndex r - shift)
                                           1 (y_row_range - 1)

                        in if candidateRow < y_row_range
                           then Just $! SparseAddress candidateRow $! 0
                           else Nothing

      maxAddress  =
        \(FormatContiguousCompressedSparseRow
            (FormatContiguousCompressedSparseInternal   y_row_range x_col_range
                                                        columnIndex rowStartIndex)) ->
                if  y_row_range < 1  || x_col_range < 1|| (V.length columnIndex  < 1)
                  then Nothing
                  else
                    let
                      -- Find last non-empty row: scan from end, or use bwdSkip
                      !lastRow = y_row_range - 1
                      !entryLast = rowStartIndex V.! lastRow
                      !candidateRow =
                        if isEmptyRowEntry entryLast
                        then let !skip = bwdSkip entryLast
                             in if skip > 0 then lastRow - skip else -1
                        else -- check if this row is actually non-empty
                          let !shift = rowStartIndex V.! 0
                          in if resolveRowStart rowStartIndex (lastRow + 1) - shift
                              > resolveRowStart rowStartIndex lastRow - shift
                             then lastRow
                             else basicHybridSearchDown
                                    (\r -> not (isEmptyRowEntry (rowStartIndex V.! r))
                                        && resolveRowStart rowStartIndex (r+1)
                                         - (rowStartIndex V.! 0)
                                         > resolveRowStart rowStartIndex r
                                         - (rowStartIndex V.! 0))
                                    0 (lastRow - 1)
                    in if candidateRow >= 0
                       then Just $! SparseAddress candidateRow $! (V.length columnIndex - 1)
                       else Nothing

       -- \ (FormatContiguousCompressedSparseRow
       -- (FormatContiguousCompressedSparseInternal _ y_range
       --          columnIndex _)) ->
       --       SparseAddress (y_range - 1) (V.length columnIndex - 1 )

  {-#  INLINE addressAsInt #-}
  addressAsInt = \ _ (SparseAddress _ addr)-> addr

  logicalForm = id

  -- For sparse formats, affine address shift walks step-by-step.
  -- Forward via nextAddr, backward via row-aware decrement.
  -- O(|step|) amortized (each row visited at most once during backward scan).
  affineAddressShift = \form@(FormatContiguousCompressedSparseRow
      (FormatContiguousCompressedSparseInternal
        _y_row_range _x_col_range columnIndex rowStartIndex))
    addr step ->
    if step == 0 then Just addr
    else if step > 0
      then let go 0 a = Just a
               go n a = case nextAddr form a of
                          Nothing -> Nothing
                          Just a' -> go (n-1) a'
           in go step addr
      else -- negative: walk backward using buffer offset and row boundaries
           let !shift = rowStartIndex V.! 0
               prevSparse (SparseAddress row pos)
                 | pos > resolveRowStart rowStartIndex row - shift =
                     -- still entries before us in this row
                     Just $! SparseAddress row (pos - 1)
                 | row <= 0 = Nothing
                 | otherwise =
                     -- find previous non-empty row
                     -- O(1) with skip encoding, linear scan fallback
                     let findPrev r
                           | r < 0 = Nothing
                           | isEmptyRowEntry (rowStartIndex V.! r) =
                               -- skip-encoded: jump backward
                               let !skip = bwdSkip (rowStartIndex V.! r)
                               in if skip > 0 && r - skip >= 0
                                  then let !targetRow = r - skip
                                           !lastPos = resolveRowStart rowStartIndex (targetRow + 1) - shift - 1
                                       in Just $! SparseAddress targetRow lastPos
                                  else Nothing
                           | resolveRowStart rowStartIndex (r+1) - shift > resolveRowStart rowStartIndex r - shift =
                               let !lastPos = resolveRowStart rowStartIndex (r+1) - shift - 1
                               in Just $! SparseAddress r lastPos
                           | otherwise = findPrev (r - 1)
                     in findPrev (row - 1)
               go 0 a = Just a
               go n a = case prevSparse a of
                          Nothing -> Nothing
                          Just a' -> go (n-1) a'
           in go (negate step) addr

  {-# INLINE toIndex #-}
  toIndex =
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
  {-# INLINE nextAddr #-}
  nextAddr =
         \ (FormatContiguousCompressedSparseRow
            (FormatContiguousCompressedSparseInternal  y_row_range _
              columnIndex rowStartIndex))
            (SparseAddress outer inner) ->
              let !shift = rowStartIndex V.! 0
                  !rowEnd = resolveRowStart rowStartIndex (outer + 1) - shift
              in if inner + 1 < rowEnd
                 then
                   -- Still within current row
                   Just (SparseAddress outer (inner+1))
                 else if inner >= (V.length columnIndex - 1)
                   then Nothing  -- last entry in entire matrix
                   else
                     -- Crossed row boundary: find next non-empty row.
                     -- With skip encoding this is O(1); without, linear scan.
                     let findNext !r
                           | r >= y_row_range = Nothing
                           | isEmptyRowEntry (rowStartIndex V.! r) =
                               let !skip = fwdSkip (rowStartIndex V.! r)
                               in if skip > 0 then findNext (r + skip)
                                  else Nothing
                           | resolveRowStart rowStartIndex (r+1) - shift > resolveRowStart rowStartIndex r - shift =
                               Just (SparseAddress r (inner + 1))
                           | otherwise = findNext (r + 1)
                     in findNext (outer + 1)


  -- {-# INLINE toAddress #-}
  toAddress =
        \ (FormatContiguousCompressedSparseRow
            (FormatContiguousCompressedSparseInternal  y_row_range x_col_range
              columnIndex rowStartIndex))
          (ix_x:*ix_y :* _ ) ->
            if  not (ix_x >= x_col_range ||  ix_y >=y_row_range )
              then
                let !rowEntry = rowStartIndex V.! ix_y
                in if isEmptyRowEntry rowEntry
                   then Nothing  -- skip-encoded empty row: O(1) bail
                   else
                     let
                       !shift = (rowStartIndex V.! 0)
                       !rowLo = rowEntry - shift
                       !rowHi = resolveRowStart rowStartIndex (ix_y + 1) - shift
                       checkIndex i =
                           if  (columnIndex V.!i) == ix_x
                             then Just i
                             else Nothing
                     in
                      (SparseAddress ix_y  <$>) $!
                         checkIndex =<<
                           lookupExactRange columnIndex ix_x rowLo rowHi

              else   (Nothing :: Maybe SparseAddress )


  -- {-# INLINE seek #-}
  {-  seek acts like a range query -- not meant for inner loops.
      Strategy: find first non-empty row at or after outerY (linear then gallop),
      then within that row find first column >= innerX (hybrid search).
      With address hint: narrow the search range when possible.
  -}
  seek =
     \form@(FormatContiguousCompressedSparseRow
              (FormatContiguousCompressedSparseInternal
                y_row_range x_col_range columnIndex rowStartIndex))
      (innerX :* outerY :*Nil) mebeSparseAddress ->
        if  not $ (innerX >=0 && innerX  < x_col_range ) && (outerY >= 0 && outerY < y_row_range)
        then Nothing
        else
          let
            !shift = rowStartIndex V.! 0
            !nnz = V.length columnIndex

            -- Given a row, return the [lo, hi) range into columnIndex.
            -- Handles skip-encoded rowptr (negative entries).
            rowRange !r = let !lo = resolveRowStart rowStartIndex r - shift
                              !hi = resolveRowStart rowStartIndex (r+1) - shift
                          in (lo, hi)

            -- Find the first non-empty row at or after startRow.
            -- O(1) when rowptr is skip-encoded (negative entries encode fwd_skip).
            -- Falls back to hybrid search for traditional rowptrs.
            findNonEmptyRow !startRow
              | startRow >= y_row_range = Nothing
              | otherwise =
                  let !entry = rowStartIndex V.! startRow
                  in if entry >= 0
                     then -- Non-negative: this row might be non-empty. Check range.
                       let !lo = entry - shift
                           !hi = resolveRowStart rowStartIndex (startRow + 1) - shift
                       in if hi > lo
                          then Just (startRow, lo, hi)
                          else -- Traditional empty row: fall back to hybrid scan
                            let !candidateRow = basicHybridSearchUp
                                  (\r -> let !rStart = resolveRowStart rowStartIndex r - shift
                                             !rEnd   = resolveRowStart rowStartIndex (r+1) - shift
                                         in rEnd > rStart)
                                  (startRow + 1) (y_row_range - 1)
                                !cLo = resolveRowStart rowStartIndex candidateRow - shift
                                !cHi = resolveRowStart rowStartIndex (candidateRow + 1) - shift
                            in if cHi > cLo
                               then Just (candidateRow, cLo, cHi)
                               else Nothing
                     else -- Skip-encoded empty row: O(1) jump forward
                       let !skip = fwdSkip entry
                       in if skip > 0 && startRow + skip < y_row_range
                          then let !targetRow = startRow + skip
                                   !lo = rowStartIndex V.! targetRow - shift
                                   !hi = resolveRowStart rowStartIndex (targetRow + 1) - shift
                               in if hi > lo
                                  then Just (targetRow, lo, hi)
                                  else Nothing  -- shouldn't happen if skip encoding is correct
                          else Nothing  -- no non-empty row forward

            -- Within a non-empty row [lo, hi), find first column >= targetCol.
            -- Returns Nothing if all columns in the row are < targetCol.
            findColGE !targetCol !lo !hi =
              let -- predicate: columnIndex[pos] >= targetCol  (monotonic: FFFFFTTTTT)
                  !pos = basicHybridSearchUp (\p -> (columnIndex V.! p) >= targetCol) lo (hi - 1)
              in if pos < hi && (columnIndex V.! pos) >= targetCol
                 then Just pos
                 else Nothing

            -- Build the result from a row and buffer position
            mkResult !row !pos = Just (toIndex form (SparseAddress row pos), SparseAddress row pos)

            -- The core search: starting from row startRow, column target innerX
            searchFrom !startRow !colTarget =
              case findNonEmptyRow startRow of
                Nothing -> Nothing
                Just (!row, !lo, !hi)
                  | row == startRow ->
                      -- Same row as target: need column >= colTarget
                      case findColGE colTarget lo hi of
                        Just !pos -> mkResult row pos
                        Nothing ->
                          -- No matching column in this row; advance to next row
                          -- and take any entry (all entries in later rows are "after" target)
                          case findNonEmptyRow (startRow + 1) of
                            Nothing -> Nothing
                            Just (!row', !lo', _hi') -> mkResult row' lo'
                  | otherwise ->
                      -- Advanced past target row; first entry in this row is valid
                      mkResult row lo

          in case mebeSparseAddress of
            Nothing -> searchFrom outerY innerX

            Just (SparseAddress hintRow hintPos)
              -- Hint is in the target row and at or past our column search start:
              -- narrow the column search within [hintPos, rowHi)
              | hintRow == outerY
              , hintPos >= 0 && hintPos < nnz
              , let (!_lo, !hi) = rowRange outerY
              , hintPos < hi ->
                  case findColGE innerX hintPos hi of
                    Just !pos -> mkResult outerY pos
                    Nothing -> searchFrom (outerY + 1) 0

              -- Hint is before our target or invalid: fall back to full search
              | otherwise -> searchFrom outerY innerX




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
--  compareIndex = \ _ as  bs ->shapeCompareRightToLeft as bs
--  {-# INLINE compareIndex#-}



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


--      {-#INLINE toIndex #-}
--      toIndex =
--       \ (FormatInnerContiguousCompressedSparseInternal _ _  _ innerDimIndex _)
--          (SparseAddress outer inner) -> (innerDimIndex V.! inner ) :* outer :*  Nil
--          -- outer is the row (y index) and inner is the lookup position for the x index



--theres 3 cases for contiguous next address:
--in the middle of a run on a fixed outer dimension,
--need to bump the outer dimension, or we're at the end of the entire array

--we make the VERY strong assumption that no illegal addresses are ever made!

--note that for very very small sparse matrices, the branching will have some
--overhead, but in general branch prediction should work out ok.

--      {-# INLINE nextAddr #-}
--      nextAddr =
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
