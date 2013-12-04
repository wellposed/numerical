{-# LANGUAGE PolyKinds   #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE CPP #-}

module Numerical.Types.Nat(Nat(..),nat,N0,N1,N2,N3,N4,N5,N6,N7,N8,N9,N10)  where
import Data.Typeable
import Data.Data 
import Language.Haskell.TH hiding (reify)

data Nat = S !Nat  | Z 
    deriving (Eq,Show,Read,Typeable,Data)    

#if defined(__GLASGOW_HASKELL_) && (__GLASGOW_HASKELL__ >= 707)
deriving instance Typeable 'Z
deriving instance Typeable 'S
#endif


-- only use this if you're ok required template haskell
nat :: Int -> TypeQ
nat n
    | n >= 0 = localNat n
    | otherwise = error "nat: negative"
    where   localNat 0 =  conT 'Z
            localNat n = conT 'S `appT` localNat (n-1)


type N0 = Z

type N1= S N0 

type N2 = S N1

type N3 = S N2 

type N4 = S N3

type N5 = S N4

type N6 = S N5

type N7 = S N6

type N8 = S N7  

type N9 = S N8

type N10 = S N9             