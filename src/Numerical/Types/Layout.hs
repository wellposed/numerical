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

{-# LANGUAGE UndecidableInstances #-}

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
data  Locality = Contiguous | Strided  | InnerContiguous

data PrimLay a  
data StaticLay a 
data Lay a 


data Row = RowS

data Col = ColS 


--data Elem ls el  where 
--    Point :: Elem '[] el
--    (:#) :: a -> Elem ls el -> Elem (a ': ls) el  


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


this actually relates to the notion of partial ordering over vectors in convex
geometry!


so roughly: we have layouts that are dense
we have layouts that can be used as tiles (and are dense)

and we have layouts which can can't be tiled, but can have elements which are tiled

So we have

PrimitiveLayouts

Static Layouts

General Layouts (which are a Top level layout over a static layout)

the Layout class tries to abstract over all three cases 
(NB: this only makes sense when the "rank" for the inner 
and outer layouts have the same rank!)

-}


{-| Sized is used as a sort of hack to make it easy to express 
   the staticly sized layouts. NB, one trade off is that its only 
   possible to express  "cube" shaped blocks, but on the other 
   hand blocking sizes are expressible for every single rank!
-}
data Sized :: * -> * where
        (:@) :: Nat -> a -> Sized a 


{-

per se I don't need the StaticLay, PrimLay, Lay constructors, BUT
I really do like how it makes things a teeny bit simpler.. though I may remove them
-}



--class SimpleDenseLayout lay (rank :: Nat) where
--  type SimpleDenseTranpose lay 
--  toIndexSimpleDense :: Shaped rank Int lay -> Shape rank Int -> Int 


class PrimLayout lay (rank :: Nat) where 
    type TranposedPrim lay 
    toIndexPrim :: Shaped rank Int (PrimLay lay) -> Shape rank Int -> Int 
    fromIndexPrim :: Shaped rank Int (PrimLay lay) -> Int -> Shape rank Int 

--{-
--primlayouts have a block size of 1, no tiling, though they may themselves
--used in forming tiles
---}


--class StaticLayout (ls :: [ Sized *]) (rank :: Nat) where
--    type TranposedStatic ls 
--    toIndexStatic :: Shaped rank Int (StaticLay ls) -> Shape rank Int -> Int 
--    fromIndexStatic :: Shaped rank Int (StaticLay ls) -> Int -> Shape rank Int     

----instance  StaticLayout [(N3 :@ Row),(N2 :@ Col)]    where 

----    deriving (Show, Read, Eq, Ord,Typeable,Data)

--class  GenLayout lay (rank :: Nat) where 
--    type TranposedGen lay 
--    toIndexGen :: Shaped rank Int (Lay lay) -> Shape rank Int -> Int
--    fromIndexGen :: Shaped rank Int (Lay lay) -> Int -> Shape rank Int 

----- not sure if I need this extra layer here
--class Layout lay (rank :: Nat) where 
--    type Tranposed lay 
--    toIndex :: Shaped rank Int lay -> Shape rank Int -> Int
--    {-# INLINE toIndex #-}
--    fromIndex :: Shaped rank Int lay -> Int -> Shape rank Int 
--    {-# INLINE fromIndex #-}
----the fact that months layer i don't understand these genlayout and static layout instances tells me something
--instance GenLayout lay rnk => Layout (Lay lay) rnk where
--    type Tranposed (Lay lay) = Lay (TranposedGen lay) 
--    toIndex = toIndexGen
--    {-# INLINE toIndex #-}
--    fromIndex = fromIndexGen
--    {-# INLINE fromIndex #-}
----
--instance StaticLayout lay rnk => Layout  (StaticLay lay) rnk where
--    type Tranposed (StaticLay lay)=  StaticLay ( TranposedStatic  lay)
--    toIndex = toIndexStatic
--    {-# INLINE toIndex #-}
--    fromIndex = fromIndexStatic
--    {-# INLINE fromIndex #-}



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


