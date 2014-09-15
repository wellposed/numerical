
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
module Numerical.Array.Range (Range(..),AffineRange(..)) where

import Data.Data
--import Data.Typeable


{-
not quite the right module for this notion of range, but lets
fix that later
-}
-- | whenever you are  tempted to
data Range a =Range {_RangeMin :: !a
                      ,_RangeMax :: !a}
        deriving (Eq,Show,Data,Typeable,Functor)

-- | this is uniform address interval by any other name
data AffineRange a = AffineRange{_AffineRangeMin :: !a
                                ,_AffineRangeStride :: ! Int
                                ,_AffineRangeMax :: !a}
        deriving (Eq,Show,Data,Typeable,Functor )