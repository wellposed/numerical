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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Numerical.Types.Shape where


import Numerical.Types.Nat 
import Data.Data
import Unsafe.Coerce





infixr 3 :*
    
 
data Shape (rank :: Nat) where 
    Nil  :: Shape Z
    (:*) :: {-# UNPACK #-} !(Int:: *) -> !(Shape r) -> Shape ( (S r))

deriving instance Show (Shape rank)

deriving instance Eq (Shape rank)


#if defined( __GLASGOW_HASKELL__ ) &&  ( __GLASGOW_HASKELL__  >= 707)
deriving instance Typeable (Shape rank)

#endif    

--deriving instance (Eq (Shape Z))


{-
maybe should just do  custom data family indexed on rank,
but lets shelve that for now, and accept that i won't quite be doing the right thing
for the initial release,

will probably want to switch to having a "Shapable" class
and have the Shape n type be a data family of sized tuples, rather than sized lists.

This is because the computations

-}

-- should the psuedo integral ops for Shape be made into a type class?
-- would enable being able to give the SPECIALIZE pragma 

addShapes :: Shape s -> Shape s -> Shape s 
addShapes Nil Nil = Nil 
addShapes (a :* as) (b :* bs) = (a+b):* (addShapes as bs)
{-# INLINABLE addShapes #-}


multShapes :: Shape s -> Shape s -> Shape s 
multShapes Nil Nil = Nil 
multShapes (a :* as) (b :* bs) = (a * b):* (addShapes as bs)

remShapes ::  Shape s -> Shape s -> Shape s 
remShapes Nil Nil = Nil 
remShapes (a :* as) (b :* bs) =  (a `rem` b) :* remShapes as bs 

modShapes ::  Shape s -> Shape s -> Shape s 
modShapes Nil Nil = Nil 
modShapes (a :* as) (b :* bs) = (a `mod` b) :* modShapes as bs 

--- the quotRem and divMod Stuff need a reverse if I do it direct style, so maybe
--- they should be done in CPS? Lets do CPS and inline dangerously :) 
--- 
---

quotRemShapes :: Shape s -> Shape s ->( Shape s ,  Shape s )
quotRemShapes Nil Nil = (Nil, Nil  )
quotRemShapes (a :* as) (b :* bs) = undefined
{-# INLINABLE quotRemShapes  #-}

divModShapes :: Shape s -> Shape s -> (Shape s ,  Shape s  )
divModShapes Nil Nil = (Nil , Nil )
divModShapes aShp@(a :* as) bShp@(b :* bs) = undefined 
{-#  INLINABLE divModShapes #-}



{-|
index ideas inspired by repa3 / repa4, but 

NOTE: for the the low rank cases (eg 1-2 dim arrays)
should figure out a lighter weight indexing story if that
winds up being widely used and blocking good fusion. Not doing this For now.

However! Writing high level algorithms in a explicitly pointwise indexing
heavy way will be a bad bad idea. 

-} 








-- using unsafe coerce as a ghetto way of writing the "proof"
-- this can be made properly type safe if i switch to >=7.8 support only,
-- though i'm not sure about the relative power weight ratio




-- reverse an Index Shape tuple    
reverseShape :: Shape r -> Shape r 
reverseShape Nil = Nil 
reverseShape !shs@(anIx :* rest) = go  shs Nil 
    where
        go :: Shape a -> Shape b -> Shape r  -- want r= PSum a b
        go !Nil !res =  unsafeCoerce $!  res  -- my "proof"
        go !(ix :* more )  !res =  go more (ix :* res)
{-# INLINABLE reverseShape #-}

{-
NB, see http://ghc.haskell.org/trac/ghc/ticket/8423
for a provable version, though only works on 7.8
and uses singletons. Should revist at some point

also see 
https://gist.github.com/cartazio/6913380 for Ranjit Jhala's formulation 
and https://gist.github.com/cartazio/6907168 for Richard Eisenberg's formulation

-}




type Zero = Z 
type One = S Zero
type Two = S One
type Three = S Two 



-- | `Shaped lay sh` is a Shape meant for representing the Array rank and dimensions sh,
-- with layout lay.
newtype Shaped lay sh = Shaped sh 

-- |  Writing down the common ranks. 
type DIM1 = Shape  One
type DIM2 = Shape  Two 
type DIM3 = Shape Three 



nilShape :: Shape $(nat 0)-> Int
nilShape _ = 0 






