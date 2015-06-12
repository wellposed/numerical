{-# LANGUAGE DataKinds, GADTs, TypeFamilies, TypeOperators,
             ConstraintKinds, ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable#-}
{-# LANGUAGE CPP #-}

module Numerical.Nat(Nat(..),N0,N1,N2,N3,N4,N5,N6,N7,N8,N9,N10
    ,SNat(..), type (+),plus_id_r,plus_succ_r,gcastWith,Proxy(..),LitNat,U)  where
import Data.Typeable
import Data.Data
import qualified GHC.TypeLits as TL


#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 707
import Data.Type.Equality(gcastWith)
#else
import Data.Proxy
#endif

type LitNat = TL.Nat

data Nat = S !Nat  | Z
    deriving (Eq,Show,Read,Typeable,Data)

#if defined(__GLASGOW_HASKELL__) && ( __GLASGOW_HASKELL__ >= 707) && ( __GLASGOW_HASKELL__ < 709)
deriving instance Typeable 'Z
deriving instance Typeable 'S
#endif

{-
use closed type families when available,
need to test that the
-}


type family U (n:: TL.Nat) :: Nat  where
  U 0 = 'Z
  U n = 'S (U (((TL.-)) n  1))


type family n1 + n2 where
  'Z + n2 = n2
  ('S n1') + n2 = 'S (n1' + n2)


--  ghc 7.6 instances

--type family U (n:: (TL.Nat)) :: Nat

---- can't induct, hence crippled
--type instance U n = Z

--type family (n1::Nat) + (n2::Nat) :: Nat
--type instance Z + n2 = n2
--type instance  (S n1) + n2 = S (n1 + n2)
--gcastWith :: (a :~: b) -> ((a ~ b) => r) -> r
--gcastWith Refl x = x
--data a :~: b where
--  Refl :: a :~: a





-- singleton for Nat


data SNat :: Nat -> * where
  SZero :: SNat 'Z
  SSucc :: SNat n -> SNat ('S n)



-- inductive proof of right-identity of +
plus_id_r :: SNat n -> ((n + 'Z) :~: n)
plus_id_r SZero = Refl
plus_id_r (SSucc n) = gcastWith (plus_id_r n) Refl

-- inductive proof of simplification on the rhs of +
plus_succ_r :: SNat n1 -> Proxy n2 -> ((n1 + ('S n2)) :~: ('S (n1 + n2)))
plus_succ_r SZero _ = Refl
plus_succ_r (SSucc n1) proxy_n2 = gcastWith (plus_succ_r n1 proxy_n2) Refl



type N0 = 'Z

type N1 = 'S N0

type N2 = 'S N1

type N3 = 'S N2

type N4 = 'S N3

type N5 = 'S N4

type N6 = 'S N5

type N7 = 'S N6

type N8 = 'S N7

type N9 = 'S N8

type N10 = 'S N9
