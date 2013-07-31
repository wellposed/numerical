{-# LANGUAGE PolyKinds   #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}


module Numerics.Types.Shape where


import Data.Data

{- index ideas inspired by repa3 -} 


-- | An index of dimension zero
data Z  = Z
    deriving (Show, Read, Eq, Ord,Typeable,Data)

-- | Our index type, used for both shapes and indices.
infixr 3 :^
data head :^ tail
    = !head :^ !tail
    deriving (Show, Read, Eq, Ord,Typeable,Data)

type DIM1 = Int:^ Z 
type DIM2 = Int :^ DIM1
type DIM3 = Int :^ DIM2    

{-| I take types to be read with an 
@ x:^ y :^ z :^ Z  
 under row major interpretation
(ie rightmost index is the least )


-}


