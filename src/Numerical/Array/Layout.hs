{-# LANGUAGE PolyKinds   #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-#  LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-#  LANGUAGE GADTs #-}
-- do i need flexible instances really?
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}


module Numerical.Array.Layout(
  Locality(..)
  ,Row(..)
  ,Column(..)
  ,Direct(..)
  ,Layout(..)) where



{-| RowMajor will be defined by a foldRight  over the  ix :^ ixes list
ColumnMajor

-}

import Numerical.Nat
import Numerical.Array.Shape as S 
import Data.Data
import qualified Data.Foldable as F 
import Control.Applicative
import Prelude hiding (foldr,foldl,foldl',map,scanl,scanr,scanl1,scanr1,scanl')

{-|  A major design goal with the Layout module is to 
make it easy to define new dense array layouts



-}
data  Locality = Contiguous | Strided  | InnerContiguous

data PrimLay a  
data StaticLay a 
data Lay a 


data Direct 

data Row 

data Column 


{-
NB: may need to add some specialization for low rank indexing, 
theres 4 choices:
a) INLINE EVERYTHING
b) rewrite rules that take low rank indexing code into specialized versions thereof
c) wait till ghc 7.8.2 to resolve https://ghc.haskell.org/trac/ghc/ticket/8848
    and use SPECIALIZE
d) benchmark and then decide

for now I choose (e), defer benchmarking till everything works :) 


a related concern is the interplay of inlining and specialization 
https://ghc.haskell.org/trac/ghc/ticket/5928

-}


getAddress :: Address -> Int 
getAddress (Address ix)=ix

newtype Address = Address  Int 
  deriving (Eq,Ord,Show,Read,Typeable,Data,Num)


data family Form lay (contiguity:: Locality)  (rank :: Nat)


class Layout lay (contiguity:: Locality) (rank :: Nat)  where
    type Tranposed lay 

    
    transposedLayout ::  (lay ~ Tranposed l2,l2~Tranposed lay)=> Form lay contiguity rank -> Form l2 contiguity rank 
    --shapeOf 
    
    basicToAddress :: Form lay contiguity rank -> Shape rank Int -> Address  

    --unchecked
    --nextAddress --- not sure if this should even exist for contiguous ones..
    -- not sure if this is the right model for the valid ops
    --validAddress::Form   lay contiguity rank -> Int -> Either String (Shape rank Int)
    --validIndex ::Form   lay contiguity rank -> Shape rank Int -> Either String Int 
    basicNextAddress :: Form   lay contiguity rank -> Address ->  Address 

    basicNextAddress =  \form shp ->  basicToAddress form $  (basicNextIndex form  $! basicToIndex form  shp )
    {-# INLINE basicNextAddress #-}
    
    basicNextIndex :: Form   lay contiguity rank -> Shape rank Int ->(Shape rank Int) 
    basicNextIndex  = \form shp ->  basicToIndex form  $  (basicNextAddress form  $! basicToAddress form  shp )
    {-# INLINE  basicNextIndex #-}


    basicToIndex :: Form   lay contiguity rank -> Address -> Shape rank Int 

    -- one of basicNextAddress and basicNextIndex must always be implemented
    {-# MINIMAL transposedLayout, basicToIndex, basicToAddress, (basicNextIndex | basicNextAddress ) #-}

-----
-----
-----

data instance Form  Direct Contiguous (S Z) = 
            FormDirectContiguous { logicalShapeDirectContiguous :: {-#UNPACK#-} !Int }

instance Layout Direct Contiguous (S Z)   where
    type Tranposed Direct = Direct


    transposedLayout = id 

    {-#INLINE basicToAddress#-}
    basicToAddress   (FormDirectContiguous _) (j :* Nil )= Address j 

    --basicNextIndex=  undefined -- \ _ x ->  Just $! x + 1 
    --note its unchecked!
    {-# INLINE basicToIndex#-}
    basicToIndex =  \ (FormDirectContiguous _) (Address ix)  -> (ix ) :* Nil 
    
    basicNextAddress = \ _ addr -> addr + 1



data instance Form  Direct Strided (S Z) = 
        FormDirectStrided { logicalShapeDirectStrided :: {-#UNPACK#-}!Int
                    , logicalStrideDirectStrided:: {-#UNPACK#-}!Int}

instance Layout Direct Strided (S Z)   where
    type Tranposed Direct = Direct


    transposedLayout = id 

    {-#INLINE basicToAddress#-}
    basicToAddress   = \ (FormDirectStrided _ strid) (j :* Nil )->  Address (strid * j) 

    {-# INLINE basicNextAddress #-}
    basicNextAddress = \ (FormDirectStrided _ strid) addr ->  addr + Address strid 

    {-# INLINE basicNextIndex#-}
    basicNextIndex =  \ _  (i:* Nil ) ->  (i + 1 :* Nil )
    

    {-# INLINE basicToIndex#-}
    basicToIndex = \ (FormDirectStrided _ stride) (Address ix)  -> (ix `div` stride ) :* Nil 

-----
-----
-----



data instance  Form  Row  Contiguous rank  = FormRowContiguous {boundsFormRow :: !(Shape rank Int)} 
-- strideRow :: Shape rank Int,
instance  Layout Row  Contiguous rank where
    type Tranposed Row = Column 

    transposedLayout = \(FormRowContiguous shp) -> FormColumnContiguous $ reverseShape shp

    {-# INLINE basicToAddress #-}
    basicToAddress = \rs tup -> let !strider =takePrefix $! S.scanr (*) 1 (boundsFormRow rs) 
                                in Address $! S.foldl'  (+) 0 $! map2 (*) strider tup 
    {-# INLINE basicNextAddress#-}                                
    basicNextAddress = \rs addr -> addr + 1 

    {-# INLINE basicToIndex #-}
    basicToIndex  =   \ rs (Address ix) -> case boundsFormRow rs of 
          Nil -> Nil 
          (_:*_)->
            let !striderShape  =takePrefix $! S.scanr (*) 1 (boundsFormRow rs) 
                in  S.map  fst $!
                            S.scanl1 (\(q,r) strid -> r `quotRem`  strid) 
                                (ix,error "impossible remainder access in Row Contiguous basicToIndex") striderShape




-----
-----
data instance  Form  Row  InnerContiguous rank  = 
        FormRowInnerContiguous {boundsFormRowInnerContig :: !(Shape rank Int), strideFormRowInnerContig:: !(Shape rank Int)} 
-- strideRow :: Shape rank Int,
instance  Layout Row  InnerContiguous rank where
    type Tranposed Row = Column 



    transposedLayout = \(FormRowInnerContiguous shp stride) -> 
        FormColumnInnerContiguous  (reverseShape shp)  (reverseShape stride)

    {-# INLINE basicToAddress #-}
    basicToAddress = \rs tup ->   Address $! S.foldl'  (+) 0 $! map2 (*) (strideFormRowInnerContig rs ) tup 

    {-# INLINE basicNextIndex #-}
    basicNextIndex = \ (FormRowInnerContiguous shape _) ix -> 
        S.map snd $! S.scanl1Zip (\( carry, oldval ) ixv shpv   -> divMod (carry + ixv) shpv ) (1,error "nextAddress init value accessed")  ix shape 

    {-# INLINE basicToIndex #-}
    basicToIndex  =   \ rs (Address ix) -> case boundsFormRowInnerContig rs of 
          Nil -> Nil 
          (_:*_)->
              S.map  fst $!
                S.scanl1 (\(q,r) strid -> r `quotRem`  strid) 
                    (ix,error "impossible remainder access in Row Contiguous basicToIndex") (strideFormRowInnerContig rs )


---
---
data instance  Form  Row  Strided rank  = 
        FormRowStrided {boundsFormRowStrided:: !(Shape rank Int), strideFormRowStrided:: !(Shape rank Int)} 
-- strideRow :: Shape rank Int,
instance  Layout Row  Strided rank where
    type Tranposed Row = Column 



    transposedLayout = \(FormRowStrided shp stride) -> 
        FormColumnStrided  (reverseShape shp)  (reverseShape stride)

    {-# INLINE basicToAddress #-}
    basicToAddress = \rs tup ->   Address $! S.foldl'  (+) 0 $! map2 (*) (strideFormRowStrided rs ) tup 

    {-#INLINE basicNextIndex#-}
    basicNextIndex = \ (FormRowStrided shape _) ix -> 
        S.map snd $! S.scanl1Zip (\( carry, oldval ) ixv shpv   -> divMod (carry + ixv) shpv ) (1,error "nextAddress init value accessed")  ix shape 

    {-# INLINE basicToIndex #-}
    basicToIndex  =   \ rs (Address ix) -> case boundsFormRowStrided rs of 
          Nil -> Nil 
          (_:*_)->
              S.map  fst $!
                S.scanl1 (\(q,r) strid -> r `quotRem`  strid) 
                    (ix,error "impossible remainder access in Row Contiguous basicToIndex") 
                    (strideFormRowStrided rs )

-----
-----
-----

data instance  Form  Column Contiguous rank  = FormColumnContiguous {boundsColumnContig :: !(Shape rank Int)}
 -- strideRow :: Shape rank Int,
instance  Layout Column  Contiguous rank where
    type Tranposed Column = Row  


    transposedLayout = \(FormColumnContiguous shp)-> FormRowContiguous $ reverseShape shp 
    {-# INLINE basicToAddress #-}
    basicToAddress    =   \ rs tup -> let !strider =  takeSuffix $! S.scanl (*) 1 (boundsColumnContig rs) 
                                in Address $! foldl' (+) 0  $! map2 (*) strider tup 
    {-# INLINE basicNextAddress #-}                                
    basicNextAddress = \ _ addr -> addr + 1                         

    {-# INLINE  basicToIndex#-}                                
    basicToIndex  = \ rs (Address ix) -> case boundsColumnContig rs of 
          Nil -> Nil 
          (_:*_)->
              let !striderShape  =takeSuffix $! S.scanl (*) 1 (boundsColumnContig rs) 
                  in S.map  fst  $! 
                        S.scanr1 (\ strid (q,r)  -> r `quotRem`  strid) 
                            (ix,error "impossible remainder access in Column Contiguous basicToIndex") striderShape




data instance  Form Column InnerContiguous rank  = FormColumnInnerContiguous {boundsColumnInnerContig :: !(Shape rank Int), strideFormColumnInnerContig:: !(Shape rank Int)}
 -- strideRow :: Shape rank Int,
instance  Layout Column  InnerContiguous rank where
    type Tranposed Column = Row  


    transposedLayout = \(FormColumnInnerContiguous shp stride)->
         FormRowInnerContiguous (reverseShape shp) (reverseShape stride) 

    {-# INLINE basicToAddress #-}
    basicToAddress    =   \ form tup -> let !strider =   strideFormColumnInnerContig form 
                                in Address $! foldl' (+) 0  $! map2 (*) strider tup 
    {-#INLINE basicNextIndex #-}                                
    basicNextIndex = \ (FormColumnInnerContiguous shape _) ix -> 
        S.map snd $! S.scanr1Zip (\ ixv shpv ( carry, oldval ) -> divMod (carry + ixv) shpv) (1,error "nextAddress init value accessed")  ix shape 


    {-# INLINE  basicToIndex#-}                                
    basicToIndex  = \ form (Address ix) -> case boundsColumnInnerContig form  of 
          Nil -> Nil 
          (_:*_)->
              let !striderShape  = strideFormColumnInnerContig form  
                  in S.map  fst  $!   S.scanr1 (\ stride (q,r)  -> r `quotRem`  stride) 
                        (ix,error "impossible remainder access in Column Contiguous basicToIndex") striderShape


data instance  Form Column Strided rank  = FormColumnStrided {boundsColumnStrided :: !(Shape rank Int), strideFormColumnStrided:: !(Shape rank Int)}
 -- strideRow :: Shape rank Int,
instance  Layout Column  Strided rank where
    type Tranposed Column = Row  


    transposedLayout = \(FormColumnStrided shp stride)->
         FormRowStrided (reverseShape shp) (reverseShape stride) 

    {-# INLINE basicToAddress #-}
    basicToAddress    =   \ form tup -> let !strider =   strideFormColumnStrided form 
                                in Address $! foldl' (+) 0  $! map2 (*) strider tup 
    {-# INLINE basicNextIndex#-}                                
    basicNextIndex = \ (FormColumnStrided shape _) ix -> 
        S.map snd $! S.scanr1Zip (\ ixv shpv ( carry, oldval ) -> divMod (carry + ixv) shpv) (1,error "nextAddress init value accessed")  ix shape 

    {-# INLINE  basicToIndex#-}                                
    basicToIndex  = \ form (Address ix) -> case boundsColumnStrided form  of 
          Nil -> Nil 
          (_:*_)->
              let !striderShape  = strideFormColumnStrided form  
                  in S.map  fst  $!   S.scanr1 (\ stride (q,r)  -> r `quotRem`  stride) 
                        (ix,error "impossible remainder access in Column Contiguous basicToIndex") striderShape



{-
*Numerical.Array.Layout> basicToAddress (FormColumn (2 :* 3 :* 7 :* Nil)) (0:* 2 :* 2 :* Nil)
Address 16
*Numerical.Array.Layout> basicToAddress (FormColumn (2 :* 3 :* 7 :* Nil)) (1:* 0 :* 0 :* Nil)
Address 1
*Numerical.Array.Layout> basicToAddress (FormColumn (2 :* 3 :* 7 :* Nil)) (0:* 0 :* 0 :* Nil)
Address 0
*Numerical.Array.Layout> basicToAddress (FormColumn (2 :* 3 :* 7 :* Nil)) (0:* 1 :* 0 :* Nil)
Address 2
*Numerical.Array.Layout> basicToAddress (FormColumn (2 :* 3 :* 7 :* Nil)) (0:* 0 :* 1 :* Nil)



-}


--data Elem ls el  where 
--    Point :: Elem '[] el
--    (:#) :: a -> Elem ls el -> Elem (a ': ls) el  


{-
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


{- Sized is used as a sort of hack to make it easy to express 
   the staticly sized layouts. NB, one trade off is that its only 
   possible to express  "cube" shaped blocks, but on the other 
   hand blocking sizes are expressible for every single rank!
-}
--data Sized :: * -> * where
    --(:@) :: Nat -> a -> Sized a 


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
-}

{-
if   tup1 is strictly less than tup2 (pointwise),
  then any lawful Layout will asign tup1 an index strictly less than that
  asigned to tup2 

  transposedLayout . transposedLayout == id 



i treat coordinates as being in x:* y :* z :* Nil, which is Fortran style idexing 

in row major we'd have for x:* y :* Nil that X is the inner dimension, and y the outter,
by contrast, in column major, y would be the inner most, and x the outter most.




-}


{- In some respects, the Layout type class is a multidimensional
analogue of the Enum type class in Haskell Prelude, 
for Dense / Dense Structured matrix formats
but 
    a) requires a witness value, the "Form"
    b) needs to handle multivariate structures
    c) has to deal with structure matrices, like triangular, symmetric, etc
    e) I think every layout should have pure 0 be a valid index, at least for "Dense" 
    arrays
    f) transposedLayout . transposedLayout == id
    g) 

  Form needs to carry the shape / extent of the matrix

-}
{-

-}

--data View = Origin | Slice 
{-
i'm really really hoping to not need a View parameter,
but the nature of the addressing logic needs to change when its a slice 
vs a deep copy (for certain classes of arrays that I wish to support very easily)

I will be likely adding this the moment benchmarks validate the distinction

on the
-}
