\begin{code}
{-# LANGUAGE DataKinds #-}
{-#  LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Numerical.Array.Address(Address(..)) where 


import Data.Data



newtype Address = Address  Int 
  deriving (Eq,Ord,Show,Read,Typeable,Data)   


instance Num Address where 
    {-# INLINE (+) #-}
    (+) (Address a) (Address b) = Address (a+b)

    {-# INLINE (-) #-}
    (-) (Address a) (Address b) =  Address (a-b)

    (*) a b = error "you cant  multiply Addresses"

    negate (Address a) = error "you cant Apply Negate to An Address"

    signum x = error "error you cant take signum of an Address"

    abs x = error "error you cant take abs of an Address"

    fromInteger x = error "you cant use Integer Literals or fromInteger to form an Address"

{-
note that 
-}
    
\end{code}