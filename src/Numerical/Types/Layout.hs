{-# LANGUAGE PolyKinds   #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-#  LANGUAGE GADTs #-}
-- do i need flexible instances really?
{-# LANGUAGE FlexibleInstances #-}

module Numerical.Types.Layout where



{-| RowMajor will be defined by a foldRight  over the  ix :^ ixes list
ColumnMajor

-}

import Numerical.Types.Nat
import Numerical.Types.Shape
import Data.Data


{-|  A major design goal with the Layout module is to 
make it easy to define new dense array layouts



-}


data PrimLay a  
data StaticLay a 
data Lay a 


data Row = RowS

data Col = ColS 


{-|
    One important invariant about all layouts at all ranks is that for 
    any given ints x < y, that the array index for inr

     toIndex shapedLayout (pure x :: Shape rank Int) is strictly less than
     toIndex shapedLayout (pure y :: Shape rank Int).

     more generally

     for rank k tuples,
      xi = x_1 :* ... :* x_k *: Nil  and   
      yj = y_1 :* ... :* x_k *: Nil 
      such that forall \ell, x_\ell  < y_\ell
    we have that
       toIndex shapedLayout xi <  toIndex  shapedLayout yj


-}


{-| Sized is used as a sort of hack to make it easy to express 
   the staticly sized layouts. NB, one trade off is that its only 
   possible to express  "cube" shaped blocks, but on the other 
   hand blocking sizes are expressible for every single rank!
-}
data Sized :: * -> * where
        (:@) :: Nat -> a -> Sized a 


{-|

per se I don't need the StaticLay, PrimLay, Lay constructors, BUT
I really do 
-}


class PrimLayout lay (rank :: Nat) where 
    type TranposedPrim lay 
    toIndexPrim :: Shaped rank Int (PrimLay lay) -> Shape rank Int -> Int 
    fromIndexPrim :: Shaped rank Int (PrimLay lay) -> Int -> Shape rank Int 


class StaticLayout (ls :: [ Sized *]) where
    type TranposedStatic
    toIndexStatic :: Shaped rank Int (StaticLay ls) -> Shape rank Int -> Int 
    fromIndexStatic :: Shaped rank Int (StaticLay ls) -> Int -> Shape rank Int     

--instance  StaticLayout [(N3 :@ Row),(N2 :@ Col)]    where 

--    deriving (Show, Read, Eq, Ord,Typeable,Data)

class  Layout lay where 
    type Tranposed lay 
    toIndex :: Shaped rank Int (Lay lay) -> Shape rank Int -> Int
    fromIndex :: Shaped rank Int (Lay lay) -> Int -> Shape rank Int 


{-
NB: I want to consider the Point a "Sized" layout of one


-}


{-data Slay :: Nat ->a->[(Nat,a)]->  * where
    Point :: Slay 1 () '[]
    Cat ::   a -> n -> Slay rank top ls -> Slay n a ((rank,top): ls)

    --- NOT SURE HOW TO WRITE THIS
-}
--data Lay a = Point  | (:#) a  Nat  (Lay a) 
    --deriving (Show, Read, Eq,Typeable,Data)

--data head :# tail =
--                     !head :# !tail 



{-
when indexing x :* h :* Nil,  we'll interpret the h as the row and the x 
as column.   So Column major forms a nice foldl (left fold), and  Row major forms
a nice 

lets try to require writing 

for now we'll distinguish between contiguous vs strided indexing, and 
for now have strided be done with  baseShape + (indexShape * strideShape),

also: should the layout 

-}


