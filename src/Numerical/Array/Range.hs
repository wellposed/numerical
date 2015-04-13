
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor, DeriveGeneric, DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable,DeriveTraversable #-}
module Numerical.Array.Range (
    Range(..)
    ,AffineRange(..)
    ,HasRange(..)
    ,affineRangeStride) where

import Data.Data
import GHC.Generics
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 709
import Data.Foldable
import Data.Traversable
#endif


{-
not quite the right module for this notion of range, but lets
fix that later
-}
-- | whenever you are  tempted to do a (lo,hi) tuple, use this instead
--  This should perhaps be made lazy, but strict for now.
data Range a =Range {_RangeMin :: !a
                      ,_RangeMax :: !a}
        deriving (Eq,Show,Data,Typeable,Generic ,Foldable,Traversable,Functor)

class HasRange r a | r -> a where
  rangeMin:: Functor f => (a -> f a )-> r -> f r
  rangeMax:: Functor f => (a -> f a )-> r -> f r

instance HasRange (Range a) a where
  rangeMax = _rangeMax
  {-#INLINE rangeMax#-}

  rangeMin = _rangeMin
  {-# INLINE rangeMin #-}

instance HasRange (AffineRange a) a where
  rangeMin = _affineRangeMin
  {-# INLINE rangeMin #-}

  rangeMax = _affineRangeMax
  {-# INLINE rangeMax #-}

_rangeMin :: Functor f => (a -> f a)-> Range a -> f (Range a)
_rangeMin = \ fun rec  -> fmap (\mup -> rec{_RangeMin= mup}) $ fun (_RangeMin rec )
{-# INLINE _rangeMin#-}

_rangeMax :: Functor f => (a -> f a) -> Range a -> f (Range a)
_rangeMax =  \ fun rec -> fmap (\mup -> rec{_RangeMax= mup}) $ fun (_RangeMax rec )
{-# INLINE _rangeMax #-}

-- | this is uniform address interval by any other name
data AffineRange a = AffineRange{_AffineRangeMin :: !a
                                ,_AffineRangeStride :: ! Int
                                ,_AffineRangeMax :: !a}
        deriving (Eq,Show,Data,Generic,Typeable,Functor,Foldable,Traversable )

_affineRangeMin :: Functor f => (a-> f a) -> AffineRange a -> f (AffineRange a)
_affineRangeMin= \ fun rec -> fmap (\mup -> rec{_AffineRangeMin=mup}) $ fun (_AffineRangeMin rec)
{-# INLINE _affineRangeMin#-}

_affineRangeMax :: Functor f => (a -> f a) -> AffineRange a -> f (AffineRange a)
_affineRangeMax= \ fun rec -> fmap (\mup -> rec{_AffineRangeMax=mup}) $ fun (_AffineRangeMax rec)
{-# INLINE _affineRangeMax #-}

affineRangeStride :: Functor f => (Int -> f Int) -> AffineRange a -> f (AffineRange a)
affineRangeStride = \fun rec  -> fmap (\mup -> rec{_AffineRangeStride=mup}) $ fun (_AffineRangeStride rec)
{-# INLINE affineRangeStride #-}
