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

module Numerical.Types.PolyShape where

import Control.Applicative
import Data.Foldable 
import Data.Monoid
import Data.Functor
import Numerical.Types.Nat 
import Prelude (seq, ($!),($),Show(..),Eq())

{-
not doing the  HLIST style shape because I don't want to have
any pattern matchings going on.

Also would play hell with locality quality in the address translation hackery,
because there'd be an extra load to get those ints!
-}
infixr 3 :*
    
 {-
the concern basically boils down to "will it specialize / inline well"

 -}
data Shape (rank :: Nat) a where 
    Nil  :: Shape Z a
    (:*) ::  !(a) -> !(Shape r a ) -> Shape  (S r) a
    
--deriving instance (Show (Shape rank a))

    -- deriving instance Eq (Shape rank a)


    -- #if defined( __GLASGOW_HASKELL__ ) &&  ( __GLASGOW_HASKELL__  >= 707)
    --deriving instance Typeable (Shape rank a)
    -- #endif    

instance Functor (Shape Z) where
    fmap  = \ f Nil -> Nil 

instance  (Functor (Shape r)) => Functor (Shape (S r)) where
    fmap  = \ f (a :* rest) -> f a :* fmap f rest 


instance  Applicative (Shape Z) where 
    pure = \ a -> Nil
    (<*>) = \ a  b -> Nil 

instance  Applicative (Shape a)=> Applicative (Shape (S a)) where     
    pure = \ a -> a :* (pure a)
    (<*>) = \ (f:* fs) (a :* as) ->  f a :* (<*>) fs as 

instance Foldable (Shape Z) where
    foldMap = \ f _ -> mempty
    foldl = \ f init  _ -> init 
    foldr = \ f init _ -> init 
    foldr' = \f !init _ -> init 
    foldl' = \f !init _ -> init   

instance (Foldable (Shape r))  => Foldable (Shape (S r)) where
    foldMap = \f  (a:* as) -> f a <> foldMap f as 
    foldl' = \f !init (a :* as) -> let   next = f  init a   in     next `seq` foldl f next as 
    foldr' = \f !init (a :* as ) -> f a $! foldr f init as               
    foldl = \f init (a :* as) -> let   next = f  init a  in   foldl f next as 
    foldr = \f init (a :* as ) -> f a $ foldr f init as     


--
map2 :: (Applicative (Shape r))=> (a->b ->c) -> (Shape r a) -> (Shape r b) -> (Shape r c )
map2 = \f l r -> pure f <*>  l  <*> r 

