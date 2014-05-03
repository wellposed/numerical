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

{-|  A major design goal with the Layout module is to
make it easy to define new dense array layouts



-}


--data PrimLay a
--data StaticLay a
--data Lay a


data Direct

data Row

data Column


{-
NB: may need to add some specialization for low rank indexing,
theres 4 choices:
a) INLINE EVERYTHING
b) rewrite rules that take low rank indexing code into specialized versions thereof
c) wait till ghc 7.8.2 to resolve https://ghc.haskell.org/trac/ghc/ticket/8848
    and use SPECIALIZE
d) benchmark and then decide

for now I choose (e), defer benchmarking till everything works :)


a related concern is the interplay of inlining and specialization
https://ghc.haskell.org/trac/ghc/ticket/5928

-}


{-
note also that this is in practice a *dense* only
layout module, though a derived api can interpret those
formats sparsely



-}

{-
TODO : move a bunch of the projection and transformation
logic from the Array classes to the analogous Layout classes.
All of the operations are oblivious to underlying buffer type

-}

-- either we need to break ties, or the ties have been broken
majorCompareLeftToRight :: Ordering -> Ordering -> Ordering
majorCompareLeftToRight EQ new = new
majorCompareLeftToRight a _ = a


majorCompareRightToLeft :: Ordering -> Ordering -> Ordering
majorCompareRightToLeft new EQ = new
majorCompareRightToLeft _ b = b

data family Format lay (contiguity:: Locality)  (rank :: Nat)


{-
LAYOUT as currently defined is actually dense layout

need to add a general sparse/dense layout format distinction


FOR NOW, punting on sparse for alpha,
will need to make breaking changes to LAYOUT later for sparse



ok, the crucial distinction between sparse and dense is that
for Dense: Index -> Address when inbounds will always succeed

but for Sparse : Index -> Address can fail even when its in bounds
-}


class Layout form rank | form -> rank where

    --type Tranposed form


    --transposedLayout ::  (form ~ Tranposed transform,transform~Tranposed form)=> form  -> transform

    -- not happy with this name, will change later FIXME TODO
    basicFormShape :: form -> Shape rank Int

    basicCompareIndex :: p form-> Shape rank Int ->Shape rank Int -> Ordering


class Layout form rank => SparseLayout  form  (rank :: Nat) | form -> rank  where



  lookupIndex :: form -> Index rank -> Maybe Address


  --note that unlike the Layout method  basicToAddress,





--class Layout form rank => DenseLayout  form  (rank :: Nat) | form -> rank  where
  {-
  empty class instances for all the dense Layouts
  -}




-----
-----
-----


instance DenseLayout (Format Direct Contiguous (S Z))  (S Z)  where



    {-#INLINE basicToAddress#-}
    basicToAddress   = \ (FormatDirectContiguous _) (j :* _ ) -> Address j

    --basicNextIndex=  undefined -- \ _ x ->  Just $! x + 1
    --note its unchecked!
    {-# INLINE basicToIndex#-}
    basicToIndex =  \ (FormatDirectContiguous _) (Address ix)  -> (ix ) :* Nil

    {-# INLINE basicNextAddress #-}
    basicNextAddress = \ _ addr -> addr + 1
