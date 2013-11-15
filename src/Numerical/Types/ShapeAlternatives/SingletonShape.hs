{-# LANGUAGE DataKinds, PolyKinds, GADTs, TypeFamilies, TypeOperators,
             ConstraintKinds, ScopedTypeVariables, RankNTypes #-}
 
module Shape where
 
import GHC.Exts
import Data.Proxy
import Data.Type.Equality
 
data Nat = Zero | Succ Nat
 
type family n1 + n2 where
  Zero + n2 = n2
  (Succ n1') + n2 = Succ (n1' + n2)
 
-- singleton for Nat
data SNat :: Nat -> * where
  SZero :: SNat Zero
  SSucc :: SNat n -> SNat (Succ n)
 
gcoerce :: (a :=: b) -> ((a ~ b) => r) -> r
gcoerce Refl x = x
 
-- inductive proof of right-identity of +
plus_id_r :: SNat n -> ((n + Zero) :=: n)
plus_id_r SZero = Refl
plus_id_r (SSucc n) = gcoerce (plus_id_r n) Refl
 
-- inductive proof of simplification on the rhs of +
plus_succ_r :: SNat n1 -> Proxy n2 -> ((n1 + (Succ n2)) :=: (Succ (n1 + n2)))
plus_succ_r SZero _ = Refl
plus_succ_r (SSucc n1) proxy_n2 = gcoerce (plus_succ_r n1 proxy_n2) Refl
 
 
data Shape :: Nat -> * where
  Nil :: Shape Zero
  (:*) :: Int -> Shape n -> Shape (Succ n)
 
reverseShape :: Shape n -> Shape n
reverseShape Nil = Nil
reverseShape list = go SZero Nil list
  where
    go :: SNat n1 -> Shape n1 -> Shape n2 -> Shape (n1 + n2)
    go snat acc Nil = gcoerce (plus_id_r snat) acc
    go snat acc (h :* (t :: Shape n3)) =
      gcoerce (plus_succ_r snat (Proxy :: Proxy n3))
              (go (SSucc snat) (h :* acc) t)

              