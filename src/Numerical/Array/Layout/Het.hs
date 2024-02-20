
{-# LANGUAGE TypeOperators #-}

module Numerical.Array.Layout.Het where 

import Numerical.Array.Layout.Base
import Data.Dynamic 


--- this operation is needed 
--- so that we can define composite formats, eg 
--- zero copy concatenations of arrays with mixed but 
--- compatible formats 
fromSomeAddress :: (Typeable addr, addr ~ LayoutAddress form ) => p form -> Dynamic -> Maybe addr
fromSomeAddress _ x = fromDynamic x
{-
The purpose of this module is to illustrate 
and substantiate zero copy vertical and horizontal 
concatenation of compatibly oriented Rectilinear formats
-}

newtype SomeAddr = MkAddr Dynamic