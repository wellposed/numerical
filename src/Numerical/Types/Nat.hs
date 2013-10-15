{-# LANGUAGE PolyKinds   #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

module Numerical.Types.Nat(Nat(..),nat)  where
import Data.Typeable
import Data.Data 
import Language.Haskell.TH hiding (reify)

data Nat = S !Nat  | Z 
    deriving (Eq,Show,Read,Typeable,Data)    

nat :: Int -> TypeQ
nat n
    | n >= 0 = localNat n
    | otherwise = error "nat: negative"
    where   localNat 0 =  conT 'Z
            localNat n = conT 'S `appT` localNat (n-1)