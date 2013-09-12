{-# LANGUAGE PolyKinds   #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}


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

-- doing the rank2 tranpose only, because transpose only really makes sense
-- in the 2d case anyways. 
tranposeIndex :: IntTuple sh => (Int :* Int :* sh) -> ( Int :* Int :* sh )
tranposeIndex (i :* j :* rs) = (j:* i :* rs)
{-# INLINE tranposeIndex#-}

class Tuple a where   

instance Tuple Z where

instance (Tuple as) => Tuple (a :* as ) where


class IntTuple a where 

instance IntTuple Z where

instance IntTuple t => IntTuple (Int :* t ) where

class  ReverseTuple  input partial where
    type RevTuple input partial
    reverseTuple :: (input ~ RevTuple res Z,   res ~ RevTuple input partial )=> input -> partial -> out 

instance Tuple partial   ReverseTuple Z partial partial where
    type RevTuple Z partial = partial   
    reverseTuple Z partial = partial

instance  (Tuple sh, Tuple (a:* sh), Tuple partial, Tuple  ) =>  ReverseTuple ( a :* sh) where
    type RevTuple (a :* sh) partialRes =  RevTuple sh (a :* partialRes)
    reverseTuple (t:* ts) partialT = reverseTuple ts (t :* partialT)





