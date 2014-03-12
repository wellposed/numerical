{-# LANGUAGE DataKinds, PolyKinds, GADTs, TypeFamilies, TypeOperators,
             ConstraintKinds, ScopedTypeVariables, RankNTypes #-}
 
module Shape where
 
import GHC.Exts
import Data.Proxy
import Data.Type.Equality
 
data Nat = Z | S Nat
 
type family n1 + n2 where
  Z + n2 = n2
  (S n1') + n2 = S (n1' + n2)
 
-- singleton for Nat
data SNat :: Nat -> * where
  SZero :: SNat Z
  SSucc :: SNat n -> SNat (S n)
 
--gcoerce :: (a :~: b) -> ((a ~ b) => r) -> r
--gcoerce Refl x = x
--gcoerce = gcastWith
 
-- inductive proof of right-identity of +
plus_id_r :: SNat n -> ((n + Z) :~: n)
plus_id_r SZero = Refl
plus_id_r (SSucc n) = gcastWith (plus_id_r n) Refl
 
-- inductive proof of simplification on the rhs of +
plus_succ_r :: SNat n1 -> Proxy n2 -> ((n1 + (S n2)) :~: (S (n1 + n2)))
plus_succ_r SZero _ = Refl
plus_succ_r (SSucc n1) proxy_n2 = gcastWith (plus_succ_r n1 proxy_n2) Refl
 
 
data Shape :: Nat -> * -> * where
  Nil :: Shape Z a
  (:*) :: a -> Shape n  a -> Shape (S n) a
 
reverseShape :: Shape n a -> Shape n a 
reverseShape Nil = Nil
reverseShape list = go SZero Nil list
  where
    go :: SNat n1 -> Shape n1  a-> Shape n2 a -> Shape (n1 + n2) a
    go snat acc Nil = gcastWith (plus_id_r snat) acc
    go snat acc (h :* (t :: Shape n3 a)) =
      gcastWith (plus_succ_r snat (Proxy :: Proxy n3))
              (go (SSucc snat) (h :* acc) t)

              