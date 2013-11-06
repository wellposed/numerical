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
{-# LANGUAGE NoImplicitPrelude #-}

module Numerical.Types.Shape where


import Numerical.Types.Nat 
import Data.Data
import Unsafe.Coerce

import Prelude  hiding (map,foldl,foldr)





--infixr 3 :*
    
 
--data Shape (rank :: Nat) where 
--    Nil  :: Shape Z
--    (:*) :: {-# UNPACK #-} !(Int:: *) -> !(Shape r) -> Shape ( (S r))

--deriving instance Show (Shape rank)

--deriving instance Eq (Shape rank)


-- #if defined( __GLASGOW_HASKELL__ ) &&  ( __GLASGOW_HASKELL__  >= 707)
--deriving instance Typeable (Shape rank)

-- #endif    

--deriving instance (Eq (Shape Z))



type Zero = Z 
type One = S Zero
type Two = S One
type Three = S Two 
type Four = S Three
type Five = S Four
type Six = S Five
type Seven = S Six
type Eight = S Seven 
type Nine = S Eight
type Ten = S Nine 


class Shapable (n :: Nat)   where 
    data Shape n :: * 
    type  Tuple n :: * 
    type TupleRank tup  :: Nat -- this should map an int tuple to their rank n 


    --type Fun n  res :: *  --- not sure if i'll include these in the final approach
                    --- but 
    --type FunRank b :: Nat 
    toShape :: (n~TupleRank tup, Tuple n ~ tup )=> tup -> Shape n 

    foldl ::  (a -> Int -> a) -> a -> Shape n -> a
    
    foldr :: (Int -> a -> a ) -> a -> Shape n -> a 
    
    -- not sure if I need the  strict variants of 
    map :: (Int -> Int) -> Shape n -> Shape n 
    zip :: (Int -> Int -> Int ) -> Shape n -> Shape n -> Shape n 
    zip2 :: (Int -> Int -> (Int ,Int ))-> Shape n -> Shape n -> (Shape n, Shape n )

-- zero rank is boring but lets include it for completeness
class Shapable Zero where
    data Shape Zero  = Shape0
    type Tuple Zero  = ()
    type TupleRank Int = Zero
    toShape = \ _ -> Shape0 
    foldl = \ f init  (Shape0) -> init 
    foldr = \ f (Shape0) init -> init 
    zip = \ f _ _ -> Shape0 
    zip2 = \f _ _ -> (Shape0,Shape0)

{-| For now I'm not making any thing strict in the shapeable instances,
except for 

-}
class Shapable  One  where
    data Shape One  = Shape1  {-# UNPACK #-}  !Int 
    type Tuple One  = Int 
    type TupleRank Int = One 
    toShape = Shape1
    foldl = \ f init  (Shape1 v1) -> f v1 init 
    foldr = \f init  (Shape1 v1) -> f init v1 
    zip = \


{-|
index ideas inspired by repa3 / repa4, but 

NOTE: for the the low rank cases (eg 1-2 dim arrays)
should figure out a lighter weight indexing story if that
winds up being widely used and blocking good fusion. Not doing this For now.

However! Writing high level algorithms in a explicitly pointwise indexing
heavy way will be a bad bad idea. 

-} 







{-
NB, see http://ghc.haskell.org/trac/ghc/ticket/8423
for a provable version, though only works on 7.8
and uses singletons. Should revist at some point

also see 
https://gist.github.com/cartazio/6913380 for Ranjit Jhala's formulation 
and https://gist.github.com/cartazio/6907168 for Richard Eisenberg's formulation

-}







---- | `Shaped lay sh` is a Shape meant for representing the Array rank and dimensions sh,
---- with layout lay.
--newtype Shaped lay sh = Shaped sh 

---- |  Writing down the common ranks. 
--type DIM1 = Shape  One
--type DIM2 = Shape  Two 
--type DIM3 = Shape Three 



--nilShape :: Shape $(nat 0)-> Int
--nilShape _ = 0 






