{-# LANGUAGE PolyKinds   #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeOperators #-}

--maybe i should just do the type familes version?
--{-# LANGUAGE  FlexibleInstances #-}
--{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE FunctionalDependencies #-}
--{-# LANGUAGE FlexibleContexts #-}
--{-# LANGUAGE UndecidableInstances #-}

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

transposeIndexTuple :: (IndexTuple a, IndexTuple b, 
            a ~ TupleTranspose b,b~TupleTranspose a  )=> it -> it
transposeIndexTuple   =  tupleReverse Z 

class IsTuple a where
    tupleReverse :: (IsTuple b, c~ TupleReverse b a,)=> b -> a  -> c

instance IsTuple Z where

instance (IsTuple as) => IsTuple (a :* as ) where


class IndexTuple a where 

instance IndexTuple Z where

instance IndexTuple t => IndexTuple (Int :* t ) where

type family TupleTranspose a

type instance TupleTranspose a = TupleReverse Z a 

type family TupleReverse res input 

type instance TupleReverse a Z = a
type instance TupleReverse a (b:* c) = TupleReverse (b :* a) c 








{-| I take types to be read with an 
@ x:^ y :^ z :^ Z  
 under row major interpretation
(ie rightmost index is the least )


-}


