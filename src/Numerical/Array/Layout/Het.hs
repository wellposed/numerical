
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

module Numerical.Array.Layout.Het where 

import Numerical.Array.Layout.Base as LB 
import Data.Dynamic 
import Data.Sequence as Seq 


{-
The purpose of this module is to illustrate 
and substantiate zero copy vertical and horizontal 
concatenation of compatibly oriented Rectilinear formats
-}

newtype SomeAddr = MkAddr Dynamic


--- you always need to do a data wrapper
--- to existentialize 
data SomeRectilinearFormat rnk orient where 
        MkSomeRect :: RectilinearLayout form rnk orient => form -> SomeRectilinearFormat rnk orient


newtype  HetRectilinearFormat rnk orient = MkHetForm (Seq.Seq (SomeRectilinearFormat rnk orient))
     -- MkHetFormat :: RectilinearLayout form rnk orient =>