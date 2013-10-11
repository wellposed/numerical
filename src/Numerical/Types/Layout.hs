{-# LANGUAGE PolyKinds   #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Numerical.Types.Layout where



{-| RowMajor will be defined by a foldRight  over the  ix :^ ixes list
ColumnMajor

-}

import Numerical.Types.Shape
import Data.Data

data Point = Point 
    deriving (Show, Read, Eq, Ord,Typeable,Data)
{-
Point is the only valid layout for a zero rank array, 

Point has a well defined shape  that is the all ones shape,
so at every rank, its the single element! 

The idea being, we have 
-}


data head :# tail =
                     !head :# !tail 
    deriving (Show, Read, Eq, Ord,Typeable,Data)

--class Shape sh => Layout a sh where
    --index ::  sh -> Shaped a sh -> Int 