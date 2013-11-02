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
so at every rank, its a single element/entry! 

The idea being, we have 
-}


data head :# tail =
                     !head :# !tail 
    deriving (Show, Read, Eq, Ord,Typeable,Data)

class Layout lay where 
    type Tranposed lay 




{-
when indexing x :* h :* Nil,  we'll interpret the h as the row and the x 
as column.   So Column major forms a nice foldl (left fold), and  Row major forms
a nice 

lets try to require writing 

for now we'll distinguish between contiguous vs strided indexing, and 
for now have strided be done with  baseShape + (indexShape * strideShape),

also: should the layout 

-}


