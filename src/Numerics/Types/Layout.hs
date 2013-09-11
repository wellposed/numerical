{-# LANGUAGE PolyKinds   #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Numerics.Types.Layout where



{-| RowMajor will be defined by a foldRight  over the  ix :^ ixes list
ColumnMajor

-}

data Point = Point 
    deriving (Show, Read, Eq, Ord,Typeable,Data)
{-
Point is the only valid layout for a zero rank array, 

Point has a well defined shape  that is the all ones shape,
so at every rank, its the single element! 
-}


data head :# tail =
                     !head :# !tail 
    deriving (Show, Read, Eq, Ord,Typeable,Data)

class Layout a where
    index :: IsTuple sh => sh -> Shape sh -> Int 