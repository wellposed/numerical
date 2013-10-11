

module Numerical.Types.Nat(Nat(..))  where

data Nat = S !Nat  | Z 
    deriving (Eq,Show,Read)    