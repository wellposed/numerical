

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}


module Numerical.Array.Locality(Locality(..),LocalityMax,LocalityMin) where

import Data.Data

data  Locality = Contiguous | Strided  | InnerContiguous
  deriving (Eq,Show,Read,Typeable,Data)

deriving instance Typeable 'Strided
deriving instance Typeable 'InnerContiguous
deriving instance  Typeable 'Contiguous

#if defined(__GLASGOW_HASKELL__) && ( __GLASGOW_HASKELL__ >= 707)
type family LocalityMax (a :: Locality) (b :: Locality)  :: Locality where
  LocalityMax  Contiguous Contiguous = Contiguous
  LocalityMax Contiguous  InnerContiguous = Contiguous
  LocalityMax Contiguous  Strided = Contiguous
  LocalityMax InnerContiguous  Contiguous  = Contiguous
  LocalityMax Strided  Contiguous  = Contiguous
  LocalityMax InnerContiguous  InnerContiguous  = InnerContiguous
  LocalityMax InnerContiguous  Strided  = InnerContiguous
  LocalityMax Strided InnerContiguous  = InnerContiguous
  LocalityMax Strided Strided = Strided
type family LocalityMin (a::Locality) (b ::Locality) :: Locality where
  LocalityMin  Contiguous Contiguous = Contiguous
  LocalityMin Contiguous  InnerContiguous = InnerContiguous
  LocalityMin Contiguous  Strided = Strided
  LocalityMin InnerContiguous  Contiguous  = InnerContiguous
  LocalityMin Strided  Contiguous  = Strided
  LocalityMin InnerContiguous  InnerContiguous  = InnerContiguous
  LocalityMin InnerContiguous  Strided  = Strided
  LocalityMin Strided InnerContiguous  = Strided
  LocalityMin Strided Strided = Strided

#else
type family LocalityMax (a :: Locality) (b :: Locality)  :: Locality
type instance  LocalityMax  a b = LocalityMaxPrivate a b

type family LocalityMaxPrivate (a :: Locality) (b :: Locality)  :: Locality
type instance  LocalityMaxPrivate  Contiguous Contiguous = Contiguous
type instance  LocalityMaxPrivate Contiguous  InnerContiguous = Contiguous
type instance  LocalityMaxPrivate Contiguous  Strided = Contiguous
type instance  LocalityMaxPrivate InnerContiguous  Contiguous  = Contiguous
type instance  LocalityMaxPrivate Strided  Contiguous  = Contiguous
type instance  LocalityMaxPrivate InnerContiguous  InnerContiguous  = InnerContiguous
type instance  LocalityMaxPrivate InnerContiguous  Strided  = InnerContiguous
type instance  LocalityMaxPrivate Strided InnerContiguous  = InnerContiguous
type instance  LocalityMaxPrivate Strided Strided = Strided

type family LocalityMin (a::Locality) (b ::Locality) :: Locality
type instance  LocalityMin a b = LocalityMinPrivate a b


type family LocalityMinPrivate (a::Locality) (b ::Locality) :: Locality
type instance  LocalityMinPrivate  Contiguous Contiguous = Contiguous
type instance  LocalityMinPrivate Contiguous  InnerContiguous = InnerContiguous
type instance  LocalityMinPrivate Contiguous  Strided = Strided
type instance  LocalityMinPrivate InnerContiguous  Contiguous  = InnerContiguous
type instance  LocalityMinPrivate Strided  Contiguous  = Strided
type instance  LocalityMinPrivate InnerContiguous  InnerContiguous  = InnerContiguous
type instance  LocalityMinPrivate InnerContiguous  Strided  = Strided
type instance  LocalityMinPrivate Strided InnerContiguous  = Strided
type instance  LocalityMinPrivate Strided Strided = Strided


#endif





