\begin{code}


{-# LANGUAGE PolyKinds   #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-#  LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-#  LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FunctionalDependencies #-}

module Numerical.Array.Layout(
  Locality(..)
  ,Format(..)
  ,Row
  ,Column
  ,Direct
  ,Layout(..)
  ,Address(..)
  ,UniformAddressInterval(..)
  ,Ordering) where



import Numerical.Nat
import Control.Applicative
import Numerical.Array.Address
import Numerical.Array.Locality
import Numerical.Array.Shape as S



import qualified Data.Foldable as F

import Prelude hiding (foldr,foldl,map,scanl,scanr,scanl1,scanr1)


class Layout form rank => LayoutBuilder form rank | form -> rank where
  type Tranposed form

  -- this  needs to be in the builder class because i need to resort
  transposedLayout ::  (form ~ Tranposed transform,transform~Tranposed form)=> form  -> transform

  -- this isn't quite right, would like to require the stream/list of indices only
  -- when dealing with s
  buildLayout :: Shape rank Int -> Maybe [Shape rank Int]->  form


\end{code}
