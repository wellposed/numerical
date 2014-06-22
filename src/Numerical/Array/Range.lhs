\begin{code}
{-# LANGUAGE DeriveDataTypeable #-}
module Numerical.Array.Range (Range(..)) where

import Data.Data
import Data.Typeable


{-
not quite the right module for this notion of range, but lets
fix that later
-}

data Range a =Range {least :: !a
                      ,greatest :: !a}
        deriving (Eq,Show,Data,Typeable)

\end{code}
