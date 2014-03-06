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
{-# LANGUAGE NoImplicitPrelude #-}

module Numerical.Types.Layout where



{-| RowMajor will be defined by a foldRight  over the  ix :^ ixes list
ColumnMajor

-}

import Numerical.Types.Nat
import Numerical.Types.Shape as S 
import Data.Data
import qualified Data.Foldable as F 
import Control.Applicative
import Prelude hiding (foldr,foldl,foldl')

{-|  A major design goal with the Layout module is to 
make it easy to define new dense array layouts



-}
data  Locality = Contiguous | Strided  | InnerContiguous

data PrimLay a  
data StaticLay a 
data Lay a 


data Direct = DirectS

data Row = RowS

data Column = ColumnS 


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


--class PrimLayout lay (rank :: Nat) where 
--    type TranposedPrim lay 
--    toIndexPrim :: Shaped rank Int (PrimLay lay) -> Shape rank Int -> Int 
--    fromIndexPrim :: Shaped rank Int (PrimLay lay) -> Int -> Shape rank Int 


{-
for now we will not deal with nested formats, but this will
be a breaking change i plan for later
-}

{-
what is the law for the Layout class?
forall valid formms
toIndex sd  (fromIndex sd ix)==ix
fromIndex sd (toIndex sd shp)==shp

if   tup1 is strictly less than tup2 (pointwise),
  then any lawful Layout will asign tup1 an index strictly less than that
  asigned to tup2 

  transposedLayout . transposedLayout == id 



i treat coordinates as being in x:* y :* z :* Nil, which is Fortran style idexing 
in row major, X would be the innermost variable because it varies over columns, Z the outtermost
in column major, Z would be the inner most, b
-}



class Layout lay (contiguity:: Locality) (rank :: Nat) where
    type Tranposed lay 
    data family Form lay contiguity (rank :: Nat)
    
    transposedLayout ::  (lay ~ Tranposed l2,l2~Tranposed lay)=> Form lay contiguity rank -> Form l2 contiguity rank 
    
    toIndex :: Form lay contiguity rank -> Shape rank Int -> Int 

    fromIndex :: Form   lay contiguity rank -> Int -> Shape rank Int 


instance Layout Direct Contiguous (S Z)   where
    type Tranposed Direct = Direct
    data Form  Direct Contiguous (S Z) = FormDirectContiguous

    transposedLayout = id 

    toIndex   FormDirectContiguous  (j :* Nil )= j 

    fromIndex FormDirectContiguous ix = (ix ) :* Nil 

instance  Layout Row  Contiguous n where
    type Tranposed Row = Column 

    data Form  Row  Contiguous rank  = FormRow {sizeRow :: Shape rank Int} -- strideRow :: Shape rank Int,

    transposedLayout = \(FormRow shp) -> FormColumn $ reverseShape shp

    toIndex rs = \tup -> let !strider = S.scanr (*) 1 (sizeRow rs) in S.foldl'  (+) 0 $! map2 (*) strider tup 

    fromIndex rs = \ix -> let !strider = S.scanr (*) 1 (sizeRow rs) in undefined



instance  Layout Column  Contiguous n where
    type Tranposed Column = Row  
    data Form  Column Contiguous rank  = FormColumn {boundsColumn :: Shape rank Int} -- strideRow :: Shape rank Int,

    transposedLayout = \(FormColumn shp)-> FormRow $ reverseShape shp 

    toIndex rs = undefined  
    --  \tup -> let !strider = S.scanr (*) 0 (boundsColumn rs) $ foldr  (+) 0  $! map2 (*) strider tup 
    fromIndex rs = undefined 
     --- \ix -> let !strider = S.scanr (*) 0 (boundsColumn rs) in undefined
