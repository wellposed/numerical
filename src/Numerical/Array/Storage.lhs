\begin{code}

{-# LANGUAGE TypeFamilies #-}
module Numerical.Array.Storage(Boxed,Unboxed,Stored,StorageVector) where

import qualified Data.Vector as BV
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Unboxed as UV


{-
not sure if this is the right design, or should just use the vector types
directly, doing this for now :)


the correct design will be to have

data family PureBuffer sort elem

data family Buffer sort st elem

and then have these be newtyped wrappers around
the corresponding vector types,

will do that tomorrow


-}

data Boxed
data Unboxed
data Stored


type family StorageVector sort  :: (  * -> * )
type instance StorageVector Boxed = BV.Vector
type instance StorageVector Unboxed = UV.Vector
type instance StorageVector Stored = SV.Vector


type family StorageVectorMut sort :: (* -> * -> *)
type instance StorageVectorMut Boxed = BV.MVector
type instance StorageVectorMut  Unboxed = UV.MVector
type instance StorageVectorMut Stored = SV.MVector




type family VectorName ( v ::  * -> * ) :: *
type instance VectorName BV.Vector = Boxed
type instance VectorName UV.Vector = Unboxed
type instance VectorName SV.Vector = Stored



type family VectorNameMut (mv :: * -> * -> * ) :: *
type instance VectorNameMut BV.MVector = Boxed
type instance VectorNameMut UV.MVector = Unboxed
type instance VectorNameMut SV.MVector = Stored
\end{code}
