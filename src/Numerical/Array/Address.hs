{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Numerical.Array.Address(
  Address(..)
  ,SparseAddress(..)
    ) where

import Data.Data
import Control.Monad (liftM)
import qualified Foreign.Storable  as Store
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as GMV
import GHC.Generics

-- | 'Address' is the type used for addressing into the underlying memory buffers
-- of numerical arrays, Used for Dense Rank n arrays, and 1dim sparse arrays.
newtype Address = Address  Int
  deriving (Eq,Ord,Show,Read,Typeable,Generic,Data,Store.Storable)

-- | 'LogicalAddress' is
-- possibly dead code
newtype LogicalAddress = LogicalAddress Int
  deriving (Eq,Ord,Show,Read,Typeable,Generic,Data,Store.Storable)
-- todo, add unboxed for


-- | this m
--newtype LogicalExtent
-- sparse address seems to be dead atm
data SparseAddress = SparseAddress {
        outerIndex  :: {-# UNPACK #-} !Int
        ,innerIndex :: {-# UNPACK #-} !Int }
      deriving (Eq,Show,Data,Generic,Typeable)

{-
At some point decouple logical and physical address
Logical Address should always be Int64 -- maybe even MORE?!
physical address should be native IntPtr (aka Int)

-}



instance Num Address where
    {-# INLINE (+) #-}
    (+) (Address a) (Address b) = Address (a+b)
    {-# INLINE (-) #-}
    (-) (Address a) (Address b) =  Address (a-b)

    (*) _ _ = error "you cant  multiply Addresses"
    negate _ = error "you cant Apply Negate to An Address"
    signum _ = error "error you cant take signum of an Address"
    abs _ = error "error you cant take abs of an Address"
    fromInteger _ = error "you cant use Integer Literals or fromInteger to form an Address"

{-
note that
-}

{-
note that i don't think these vector instances ever matter
-}

newtype instance UV.MVector s Address  = MV_Address (UV.MVector s Int)
newtype instance UV.Vector Address  = V_Address  (UV.Vector    Int)

instance UV.Unbox Address where



instance  GMV.MVector UV.MVector Address where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}
  {-# INLINE basicInitialize #-}
  basicInitialize = \ (MV_Address mva) -> GMV.basicInitialize mva
  basicLength (MV_Address v) = GMV.basicLength v
  basicUnsafeSlice i n (MV_Address v) = MV_Address $ GMV.basicUnsafeSlice i n v
  basicOverlaps (MV_Address v1) (MV_Address v2) = GMV.basicOverlaps v1 v2
  basicUnsafeNew n = MV_Address `liftM` GMV.basicUnsafeNew n
  basicUnsafeReplicate n (Address a) = MV_Address `liftM` GMV.basicUnsafeReplicate n a
  basicUnsafeRead (MV_Address v) i = Address `liftM` GMV.basicUnsafeRead v i
  basicUnsafeWrite (MV_Address v) i (Address a) = GMV.basicUnsafeWrite v i a
  basicClear (MV_Address v) = GMV.basicClear v
  basicSet (MV_Address v) (Address a) = GMV.basicSet v a
  basicUnsafeCopy (MV_Address v1) (MV_Address v2) = GMV.basicUnsafeCopy v1 v2
  basicUnsafeMove (MV_Address v1) (MV_Address v2) = GMV.basicUnsafeMove v1 v2
  basicUnsafeGrow (MV_Address v) n = MV_Address `liftM` GMV.basicUnsafeGrow v n

instance  GV.Vector UV.Vector Address where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MV_Address v) = V_Address `liftM` GV.basicUnsafeFreeze v
  basicUnsafeThaw (V_Address v) = MV_Address`liftM` GV.basicUnsafeThaw v
  basicLength (V_Address v) = GV.basicLength v
  basicUnsafeSlice i n (V_Address v) = V_Address $ GV.basicUnsafeSlice i n v
  basicUnsafeIndexM (V_Address v) i
                = Address `liftM` GV.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_Address mv) (V_Address v)
                = GV.basicUnsafeCopy mv v
  elemseq _ (Address a) z =   GV.elemseq (undefined :: UV.Vector a) a z

