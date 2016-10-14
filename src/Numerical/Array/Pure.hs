

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables#-}
-- {-#  LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-#  LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-#  LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FunctionalDependencies #-}

module Numerical.Array.Pure      where


--import Numerical.Array.Address
import qualified Numerical.Array.Layout as L

import Numerical.Array.Locality
import Numerical.Array.Shape
import Numerical.Array.Range
import Numerical.Array.Storage as S
import Numerical.World

import qualified Data.Vector.Generic as VG


{-
a general question that you might ask is "what primops need have a monad constraint"

ie rather than having type a -> b, why are they type Monad m => a -> m b ?

the answer boils down to the following: most array types have
a *PURE* header data structure that can't be mutated,
that contains the Shape, extent, some handle/pointer to the associated underlying
buffer/datastructure.  Any (even nominally pure) access to  that potentially
mutable buffer should be mediated by a monad.

I further assume that the *structure* and *extent* of this underlying buffer cannot change.

That is, A valid address will always stay valid, even if after some mutation it may
    correspond to a *different* index than it did before.
-}

{-
Fix ME, these names are lame

ImmArray == immutable array

-}
data family ImmArray world rep lay (view::Locality) (rank :: Nat )   el

data instance  ImmArray Native rep lay locality rank  el =
  ImMutableNativeArray {
          nativeBufferPure  :: ! (S.BufferPure rep  el  )
          ,nativeFormatPure :: ! (L.Format lay locality rank rep)
    }


class  PureArray arr   (rank:: Nat)   a |  arr -> rank   where
    type PureArrayAddress (arr :: *  -> * ) ::  *

    -- | gives the shape, a 'rank' length list of the dimensions
    basicShape :: arr   a -> Index rank

    --basicUnsafeRead  :: PrimMonad m => marr  (PrimState m)   a -> Shape rank Int -> m (Maybe a)

    --  | basicMutableSparseIndexToAddres checks if a index is present or not
    -- helpful primitive for authoring codes for (un)structured sparse array format
    -- FIXME : THIS IS A TERRIBLE NAME
    basicSparseIndexToAddress :: ( address ~PureArrayAddress  arr) => arr a -> Index rank  -> (Maybe address)

    -- |
    basicAddressToIndex :: (address ~PureArrayAddress  arr) => arr a -> address ->  (Index rank  )

    -- |  return the Range of valid logical addresses
    basicAddressRange :: (address ~PureArrayAddress  arr)=>  arr a -> Maybe (Range address)



    -- | gives the next valid logical address
    -- undefined on invalid addresses and the greatest valid address.
    -- Note that for invalid addresses in between minAddress and maxAddress,
    -- will return the next valid address
    basicNextAddress :: (address ~PureArrayAddress  arr)=>  arr a -> address -> Maybe address

    -- I think the case could be made for a basicPreviousAddress opeeration

    -- | gives the next valid array index
    -- undefined on invalid indices and the greatest valid index
    basicNextIndex :: (address ~PureArrayAddress  arr)=>
      arr a ->  Index rank -> Maybe address  -> Maybe ( Index rank, address)


    -- | for a given valid address, @'basicAddressRegion' addr @ will return an AddressInterval
    -- that contains @addr@. This will be a singleton when the "maximal uniform stride interval"
    -- containing @addr@ has strictly less than 3 elements. Otherwise will return an Address range
    -- covering the maximal interval that will have cardinality at least 3.


    --basicAddressRegion :: (address ~PureArrayAddress  arr)=>  arr   a -> address ->  UniformAddressInterval address

    ---- | Yield the element at the given position. This method should not be
    ---- called directly, use 'unsafeRead' instead.
    basicUnsafeAddressRead  :: (Monad m , address ~PureArrayAddress  arr)=>  arr   a -> address-> m  a



    -- | Yield the element at the given position. This method should not be
    -- called directly, use 'unsafeSparseRead' instead.
    basicUnsafeSparseRead  :: Monad m => arr   a -> Index rank  -> m (Maybe a)

-- the catch all layout instance

instance (Buffer rep el , L.Layout (L.Format  lay locality  rank rep) rank)
  =>PureArray (ImmArray Native rep lay locality rank )   rank el   where
      type PureArrayAddress (ImmArray Native rep lay locality rank )
        =L.LayoutAddress (L.Format lay locality rank rep)

      {-# INLINE basicShape #-}
      basicShape = L.basicLogicalShape . nativeFormatPure

      {-# INLINE basicSparseIndexToAddress #-}
      basicSparseIndexToAddress= L.indexToAddress . nativeFormatPure

      {-#  INLINE basicAddressToIndex #-}
      basicAddressToIndex = L.addressToIndex . nativeFormatPure

      {-# INLINE basicAddressRange #-}
      basicAddressRange = L.basicAddressRange . nativeFormatPure

      {-# INLINE basicNextAddress #-}
      basicNextAddress= L.basicNextAddress . nativeFormatPure

      {-# INLINE basicNextIndex #-}
      basicNextIndex = L.basicNextIndex . nativeFormatPure

      {-# INLINE basicUnsafeSparseRead #-}
      basicUnsafeSparseRead =
          \ arr ix -> case basicSparseIndexToAddress arr ix of
                          Nothing -> return Nothing
                          (Just addr) ->  basicUnsafeAddressRead arr addr >>= ( return . Just)

      {-# INLINE basicUnsafeAddressRead #-}
      basicUnsafeAddressRead =
          \ arr  addr ->
            VG.basicUnsafeIndexM (nativeBufferPure arr)
                (L.basicAddressAsInt (nativeFormatPure arr) $ addr)

class PureArray arr rank a => PureDenseArray arr rank a where

    -- | 'basicIndexInBounds' is an O(1) bounds check.
    basicIndexInBounds :: arr a -> Index rank -> Bool

    -- |
    basicUnsafeAddressDenseRead  :: (address ~PureArrayAddress  arr,Monad m) => arr  a -> address-> m a



    -- | Yield the element at the given position. This method should not be
    -- called directly, use 'unsafeRead' instead.
    basicUnsafeDenseReadM  :: Monad m =>  arr     a -> Index rank  -> m a






