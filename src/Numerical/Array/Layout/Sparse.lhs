
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


{-# LANGUAGE PolyKinds   #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE StandaloneDeriving#-}

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
import Numerical.Array.Layout.Base
import Numerical.Array.Shape
import Numerical.Array.Address


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
      ,logicalBaseShiftDirectSparse::{-# UNPACK#-} !Int
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
data instance Format CompressedSparseRow Contiguous (S (S Z)) rep =
    FormatContiguousCompressedSparseRow {
      logicalRowShapeContiguousCSR ::  {-# UNPACK #-} !Int
      ,logicalColumnShapeContiguousCSR ::  {-# UNPACK #-} !Int
      ,logicalValueBufferAddressShiftContiguousCSR:: {-# UNPACK #-} !Int
      ,logicalColumnIndexContiguousCSR :: !(StorageVector rep Int)
      ,logicalRowStartIndexContiguousCSR :: ! (StorageVector rep Int )
  }
--deriving instance (Show (Shape (S (S Z)) Int), Show (StorageVector rep Int) ) => Show (Format CompressedSparseRow Contiguous (S (S Z)) rep)

--deriving instance  (Eq (Shape (S (S Z)) Int), Eq (StorageVector rep Int) ) => Eq (Format CompressedSparseRow Contiguous (S (S Z)) rep)

--deriving instance (Data (Shape (S (S Z)) Int), Data (StorageVector rep Int) ) => Data (Format CompressedSparseRow Contiguous (S (S Z)) rep)

--deriving instance  (Typeable (Shape (S (S Z)) Int ), Typeable (StorageVector rep Int) ) => Typeable (Format CompressedSparseRow Contiguous (S (S Z)) rep)
    --deriving (Eq,Data,Typeable)

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

    basicToSparseAddress :: (address ~ SparseLayoutAddress form)=>form  -> Shape rank Int -> Maybe  address


    basicToSparseIndex ::(address ~ SparseLayoutAddress form)=> form -> address -> Shape rank Int


    basicNextAddress :: (address ~ SparseLayoutAddress form)=>form  -> address -> Maybe  address

    basicNextIndex :: form  -> Shape rank Int -> Maybe  (Shape rank Int)



\end{code}



CSR and CSC go here, and their version of lookups and next address and next index
