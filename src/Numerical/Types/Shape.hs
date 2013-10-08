{-# LANGUAGE PolyKinds   #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
--{-# LANGUAGE MultiParamTypeClasses #-}

module Numerical.Types.Shape where


import Data.Data


infixr 3 :*

 
data Shape (sh :: [*]) where 
    Nil  :: Shape ( '[] )
    (:*) :: !(a:: *) -> !(Shape sh) -> Shape (a ': sh)

{-|
index ideas inspired by repa3 / repa4

NOTE: for the the low rank cases (eg 1-2 dim arrays)
should figure out a lighter weight indexing story if that
winds up being widely used and blocking good fusion. Not doing this For now.

However! Writing high level algorithms in a explicitly pointwise indexing
heavy way will be a bad bad idea. 

-} 

-- doing the rank2 tranpose only, because transpose only really makes sense
-- in the 2d case anyways. 
--tranposeIndex :: IntShape sh => (Int :* Int :* sh) -> ( Int :* Int :* sh )
--tranposeIndex (i :* j :* rs) = (j:* i :* rs)
--{-# INLINE tranposeIndex#-}







-- | `Shaped lay sh` is a Shape meant for representing the Array rank and dimensions sh,
-- with layout lay.
newtype Shaped lay sh = Shaped sh 

-- |  Writing down the common ranks. 
type DIM1 = Shape '[Int] 
type DIM2 = Shape [Int,Int]
type DIM3 = Shape [Int,Int,Int] 


{-| 'Shape' is our way of typing our hlist tuples, rulling out eg 
    
 -}







