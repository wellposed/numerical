{-# LANGUAGE PolyKinds   #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeOperators #-}

--maybe i should just do the type familes version?
{-# LANGUAGE  FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE  ScopedTypeVariables #-}



module Numerics.Types.Tuple where


import Data.Data

{-|
index ideas inspired by repa3 / repa4

NOTE: for the the low rank cases (eg 1-2 dim arrays)
should figure out a lighter weight indexing story if that
winds up being widely used and blocking good fusion. Not doing this For now.

However! Writing high level algorithms in a explicitly pointwise indexing
heavy way will be a bad badd idea. 

-} 


--  | An shape/index of dimension zero
data Z  = Z
    deriving (Show, Read, Eq, Ord,Typeable,Data)

--  | Our index/shape type, used for both shapes and indices.
infixr 3 :*
data head :* tail
    = !head :* !tail
    deriving (Show, Read, Eq, Ord,Typeable,Data)


-- |  Writing down the common ranks. 
type DIM1 = Int:* Z 
type DIM2 = Int :* DIM1
type DIM3 = Int :* DIM2    


{-| 'IsTuple' is our way of typing our hlist tuples, rulling out eg 
    
 -}

transposeIndexTuple :: (IndexTuple it )=> it -> it
transposeIndexTuple   =  tupleReverse

class IsTuple a where

instance IsTuple Z where

instance (IsTuple as) => IsTuple (a :* as ) where


class IndexTuple a where 

instance IndexTuple Z where

instance IndexTuple t => IndexTuple (Int :* t ) where



class TupleReverse l1 l2 | l1 -> l2, l2 -> l1  where
  tupleReverse:: l1 -> l2

instance (TupleReverse' Z l2 l3, TupleReverse' Z l3 l2) =>  TupleReverse l2 l3 where
  tupleReverse l1 = hReverse' Z l1
  {-# INLINE tupleReverse#-}


-- l3 = (reverse l2) ++ l1

class TupleReverse' l1 l2 l3 | l1 l2 -> l3 where
    hReverse':: l1 -> l2 -> l3

instance TupleReverse' l1 Z l1  where

    hReverse' l1 Z = l1
    {-# INLINE hReverse' #-}

instance TupleReverse' ( a :* l1) l2' l3  => TupleReverse' l1 ( a  :*l2') l3  where
    hReverse' l1 (a :* l2') = hReverse' ( a :* l1) l2'
    {-# INLINE hReverse' #-}






{-| I take types to be read with an 
@ x:^ y :^ z :^ Z  
 under row major interpretation
(ie rightmost index is the least )


-}


