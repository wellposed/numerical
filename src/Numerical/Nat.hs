{-# LANGUAGE DataKinds, PolyKinds, GADTs, TypeFamilies, TypeOperators,
             ConstraintKinds, ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable#-}
{-# LANGUAGE CPP #-}

module Numerical.Nat(Nat(..),nat,N0,N1,N2,N3,N4,N5,N6,N7,N8,N9,N10
    ,SNat(..), type (+),plus_id_r,plus_succ_r,gcastWith,Proxy(..),LitNat,U)  where
import Data.Typeable
import Data.Data
import qualified GHC.TypeLits as TL
import Language.Haskell.TH hiding (reify)

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 707
import Data.Type.Equality(gcastWith)
#else
import Data.Proxy
#endif

type LitNat = TL.Nat

data Nat = S !Nat  | Z
    deriving (Eq,Show,Read,Typeable,Data)

#if defined(__GLASGOW_HASKELL_) && __GLASGOW_HASKELL__ >= 707
deriving instance Typeable 'Z
deriving instance Typeable 'S
#endif


{-
use closed type families when available,
need to test that the
-}

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 707

type family U (n:: TL.Nat) :: Nat  where
  U 0 = Z
  U n = S (U (((TL.-)) n  1))


type family n1 + n2 where
  Z + n2 = n2
  (S n1') + n2 = S (n1' + n2)

#else

type family U (n:: (TL.Nat)) :: Nat

-- can't induct, hence crippled
type instance U n = Z

type family (n1::Nat) + (n2::Nat) :: Nat
type instance Z + n2 = n2
type instance  (S n1) + n2 = S (n1 + n2)
gcastWith :: (a :~: b) -> ((a ~ b) => r) -> r
gcastWith Refl x = x
data a :~: b where
  Refl :: a :~: a
#endif




-- singleton for Nat


data SNat :: Nat -> * where
  SZero :: SNat Z
  SSucc :: SNat n -> SNat (S n)



-- inductive proof of right-identity of +
plus_id_r :: SNat n -> ((n + Z) :~: n)
plus_id_r SZero = Refl
plus_id_r (SSucc n) = gcastWith (plus_id_r n) Refl

-- inductive proof of simplification on the rhs of +
plus_succ_r :: SNat n1 -> Proxy n2 -> ((n1 + (S n2)) :~: (S (n1 + n2)))
plus_succ_r SZero _ = Refl
plus_succ_r (SSucc n1) proxy_n2 = gcastWith (plus_succ_r n1 proxy_n2) Refl

-- only use this if you're ok required template haskell
nat :: Int -> TypeQ
nat n
    | n >= 0 = localNat n
    | otherwise = error "nat: negative"
    where   localNat 0 =  conT 'Z
            localNat m = conT 'S `appT` localNat (m-1)


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
