% 
\begin{code}

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

module Numerical.Array.Generic.Pure      where 


import Numerical.Array.Address 
import Numerical.Array.DenseLayout (Address(..),Locality(..),Direct(..))
import Numerical.Array.Shape 


class  Array arr   (rank:: Nat)   a |  arr -> rank   where
     
    -- | gives the shape, a 'rank' length list of the dimensions
    basicShape :: arr   a -> Shape rank Int 

    --basicUnsafeRead  :: PrimMonad m => marr  (PrimState m)   a -> Shape rank Int -> m (Maybe a)

    --  | basicMutableSparseIndexToAddres checks if a index is present or not
    -- helpful primitive for authoring codes for (un)structured sparse array format
    basicSparseIndexToAddress ::  arr a -> Shape rank Int ->  Maybe Address

    -- | 
    basicAddressToIndex :: arr a -> Address ->   Shape rank Int 

    -- |  return the smallest valid logical address
    basicSmallestAddress ::  arr a ->  Address 

    --  | return the largest valid logical ad
    basicGreatestAddress ::  arr a ->  Address 

    -- |  return the smallest valid array index
    --  should be weakly dominated by every other valid index
    basicSmallestIndex :: arr a -> (Shape rank Int)

    -- | return the greatest valid array index
    -- should weakly dominate every 
    basicGreatestIndex ::  arr a -> (Shape rank Int)

    -- | gives the next valid logical address 
    -- undefined on invalid addresses and the greatest valid address.
    -- Note that for invalid addresses in between minAddress and maxAddress,
    -- will return the next valid address 
    basicNextAddress ::  arr a -> Address -> Address 

    -- I think the case could be made for a basicPreviousAddress opeeration

    -- | gives the next valid array index
    -- undefined on invalid indices and the greatest valid index 
    basicNextIndex ::  arr a -> (Shape rank Int) -> (Shape rank Int )

    -- | for a given valid address, @'basicAddressRegion' addr @ will return an AddressInterval  
    -- that contains @addr@. This will be a singleton when the "maximal uniform stride interval"
    -- containing @addr@ has strictly less than 3 elements. Otherwise will return an Address range
    -- covering the maximal interval that will have cardinality at least 3.
    basicAddressRegion ::  arr   a -> Address ->  AddressInterval 



    ---- | Yield the element at the given position. This method should not be
    ---- called directly, use 'unsafeRead' instead.
    basicUnsafeAddressRead  ::  arr   a -> Address-> a



    -- | Yield the element at the given position. This method should not be
    -- called directly, use 'unsafeSparseRead' instead.
    basicUnsafeSparseRead  ::  marr   a -> Shape rank Int -> Maybe a

    


    
\end{code}