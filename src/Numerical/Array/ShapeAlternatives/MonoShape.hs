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

module Numerical.Types.MonoShape where


import Numerical.Types.Nat 

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
data Shape (rank :: Nat) where 
    Nil  :: Shape Z
    (:*) :: {-# UNPACK #-} !(Int:: *) -> !(Shape r) -> ShapeAlt ( (S r))

deriving instance Show (ShapeAlt rank)

deriving instance Eq (ShapeAlt rank)


#if defined( __GLASGOW_HASKELL__ ) &&  ( __GLASGOW_HASKELL__  >= 707)
deriving instance Typeable (Shape rank)

#endif    