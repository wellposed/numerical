\begin{code}

{-# LANGUAGE TypeFamilies #-}
module Numerical.Array.Storage(Boxed
  ,Unboxed
  ,Storable
  ,BufferPure(..)
  ,BufferMut(..)) where

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
data Storable


data family   BufferPure sort  elem
newtype instance BufferPure Boxed elem = BoxedBuffer (BV.Vector elem)
newtype instance BufferPure Unboxed elem = UnboxedBuffer (UV.Vector elem)
newtype instance BufferPure Storable elem = StorableBuffer (SV.Vector elem)


data family   BufferMut sort st elem
newtype instance BufferMut Boxed st   elem = BoxedBufferMut (BV.MVector st elem)
newtype instance BufferMut Unboxed st elem = UnboxedBufferMut (UV.MVector st elem)
newtype instance BufferMut Storable st  elem = StorableBufferMut (SV.MVector st elem)




\end{code}
