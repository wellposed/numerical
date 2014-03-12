% 
\begin{code}
module Numerical.Array.Generic.Mutable(MArray(..)) where    

import Control.Monad.Primitive ( PrimMonad, PrimState )  
import Numerical.Array.Layout  
\end{code}
-- 
-- For now we're going to just crib the vector style api and Lift it 
-- up into a multi dimensional setting. 
-- 
-- the tentative design is to have something like 
\begin{code}
    
data MArray s world rep lay (view:: Locality) rank elm where
     MArray  
         {marrBuffer :: {-# UNPACK #-}!(Buffer  world rep elm) 
         ,marrForm :: {-# UNPACK #-} !(Form lay view rank) } 
\end{code}

The key idea is 