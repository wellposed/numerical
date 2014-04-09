\begin{code}
{-# LANGUAGE DataKinds #-}
{-#  LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Numerical.Array.Address(Address(..)) where 


import Data.Data



newtype Address = Address  Int 
  deriving (Eq,Ord,Show,Read,Typeable,Data,Num)   
    
\end{code}