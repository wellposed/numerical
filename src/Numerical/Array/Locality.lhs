\begin{code}

{-# LANGUAGE DataKinds #-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}

module Numerical.Array.Locality(Locality(..),LocalityMax,LocalityMin) where

import Data.Data

data  Locality = Contiguous | Strided  | InnerContiguous
  deriving (Eq,Show,Read,Typeable,Data)



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
type instance  LocalityMax  Contiguous Contiguous = Contiguous
type instance  LocalityMax Contiguous  InnerContiguous = Contiguous
type instance  LocalityMax Contiguous  Strided = Contiguous
type instance  LocalityMax InnerContiguous  Contiguous  = Contiguous
type instance  LocalityMax Strided  Contiguous  = Contiguous
type instance  LocalityMax InnerContiguous  InnerContiguous  = InnerContiguous
type instance  LocalityMax InnerContiguous  Strided  = InnerContiguous
type instance  LocalityMax Strided InnerContiguous  = InnerContiguous
type instance  LocalityMax Strided Strided = Strided

type family LocalityMin (a::Locality) (b ::Locality) :: Locality
type instance  LocalityMin  Contiguous Contiguous = Contiguous
type instance  LocalityMin Contiguous  InnerContiguous = InnerContiguous
type instance  LocalityMin Contiguous  Strided = Strided
type instance  LocalityMin InnerContiguous  Contiguous  = InnerContiguous
type instance  LocalityMin Strided  Contiguous  = Strided
type instance  LocalityMin InnerContiguous  InnerContiguous  = InnerContiguous
type instance  LocalityMin InnerContiguous  Strided  = Strided
type instance  LocalityMin Strided InnerContiguous  = Strided
type instance  LocalityMin Strided Strided = Strided

#endif


\end{code}



