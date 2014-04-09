\begin{code}

{-# LANGUAGE DataKinds #-}

{-# LANGUAGE DeriveDataTypeable #-}

module Numerical.Array.Locality(Locality(..)) where 

import Data.Data

data  Locality = Contiguous | Strided  | InnerContiguous
  deriving (Eq,Show,Read,Typeable,Data)  
    
\end{code}



