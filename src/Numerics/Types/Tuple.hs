{-# LANGUAGE PolyKinds   #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeOperators #-}
--{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-#LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}


module Numerics.Types.Tuple where


import Data.Data

{-|
index ideas inspired by repa3 / repa4

NOTE: for the the low rank cases (eg 1-2 dim arrays)
should figure out a lighter weight indexing story if that
winds up being widely used and blocking good fusion. Not doing this For now.

However! Writing high level algorithms in a explicitly pointwise indexing
heavy way will be a bad bad idea. 

-} 

-- doing the rank2 tranpose only, because transpose only really makes sense
-- in the 2d case anyways. 
tranposeIndex :: IntTuple sh => (Int :* Int :* sh) -> ( Int :* Int :* sh )
tranposeIndex (i :* j :* rs) = (j:* i :* rs)
{-# INLINE tranposeIndex#-}


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


-- | `Tuple a` is checking that our shape/index tuples are proper Hlists
class Tuple a where   
instance Tuple Z where
instance (Tuple as) => Tuple (a :* as ) where

-- | `IntTuple a` checks that our shape tuples are only integers
class IntTuple a where 
instance IntTuple Z where
instance IntTuple t => IntTuple (Int :* t ) where

--- not sure right now if this type family is a good idea or not
type ReverseTuple input = ReverseTuple' input Z 

type family ReverseTuple' input partial
type instance ReverseTuple' (a:* b) partial = ReverseTuple' b (a:* partial)
type instance ReverseTuple' Z res = res 


-- HList style reverse as a fundeps type class, simplest way to write ti
-- actually from hlist

class HReverse l1 l2 | l1 -> l2, l2 -> l1 where
    hReverse:: l1 -> l2

instance (HReverse' Z l2 l3, HReverse' Z l3 l2) =>  HReverse l2 l3 where
    hReverse l1 = hReverse' Z l1
    {-#INLINE hReverse#-}



class HReverse' l1 l2 l3 | l1 l2 -> l3  where
    hReverse':: l1 -> l2 -> l3

instance HReverse' l1 Z l1 where
    hReverse' !l1 Z = l1
    {-# INLINE hReverse' #-}

instance HReverse' ( a  :* l1) l2' l3 => HReverse' l1 (a :* l2') l3 where
    hReverse' l1 !( a :* l2') = hReverse' ( a :*  l1) l2'
    {-# INLINE hReverse' #-}



