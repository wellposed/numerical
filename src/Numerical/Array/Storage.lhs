\begin{code}

{-# LANGUAGE TypeFamilies,FlexibleInstances,MultiParamTypeClasses,FlexibleContexts #-}
module Numerical.Array.Storage(Boxed
  ,Unboxed
  ,Storable
  ,BufferPure(..)
  ,BufferMut(..)) where

--import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector as BV
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Unboxed as UV
--import qualified Data.Vector.Mutable as BVM
--import qualified Data.Vector.Storable.Mutable as SVM
--import qualified Data.Vector.Unboxed.Mutable as UVM

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


instance (VGM.MVector BV.MVector elem) => VGM.MVector (BufferMut Boxed)  elem where
  basicLength = \(BoxedBufferMut v) -> VGM.basicLength v

  basicUnsafeSlice =
    \ ix1 ix2 (BoxedBufferMut bv) ->
      BoxedBufferMut $ VGM.basicUnsafeSlice ix1 ix2 bv

  basicOverlaps =
    \ (BoxedBufferMut bv1) (BoxedBufferMut bv2) -> VGM.basicOverlaps bv1 bv2

  basicUnsafeNew = \ size ->
      do
        res<- VGM.basicUnsafeNew size
        return  (BoxedBufferMut res)

  basicUnsafeRead= \(BoxedBufferMut bv) ix -> VGM.basicUnsafeRead bv ix

  basicUnsafeWrite = \(BoxedBufferMut bv ) ix val -> VGM.basicUnsafeWrite bv ix val
--  basicUnsafeClear
--  basicUnsafeSet
--  basicUnsafeCopy
--  basicUnsafeMove
--  basicUnsafeGrow
--  basicUnsafeReplicate

  {-#INLINE basicLength#-}
  {-#INLINE basicUnsafeSlice#-}
  {-#INLINE basicOverlaps#-}
  {-#INLINE basicUnsafeNew#-}
  {-#INLINE basicUnsafeRead#-}
  {-#INLINE basicUnsafeWrite#-}

--  {-# INLINE basicUnsafeClear#-}
--  {-# INLINE basicUnsafeSet#-}
--  {-# INLINE basicUnsafeCopy#-}
--  {-# INLINE basicUnsafeMove#-}
--  {-# INLINE basicUnsafeGrow#-}
--  {-# INLINE basicUnsafeReplicate#-}

instance (VGM.MVector SV.MVector elem) => VGM.MVector (BufferMut Storable)  elem where
  basicLength = \(StorableBufferMut v) -> VGM.basicLength v

  basicUnsafeSlice =
    \ ix1 ix2 (StorableBufferMut bv) ->
      StorableBufferMut $ VGM.basicUnsafeSlice ix1 ix2 bv

  basicOverlaps =
    \ (StorableBufferMut bv1) (StorableBufferMut bv2) -> VGM.basicOverlaps bv1 bv2

  basicUnsafeNew = \ size ->
      do
        res<- VGM.basicUnsafeNew size
        return  (StorableBufferMut res)

  basicUnsafeRead= \(StorableBufferMut bv) ix -> VGM.basicUnsafeRead bv ix

  basicUnsafeWrite = \(StorableBufferMut bv ) ix val -> VGM.basicUnsafeWrite bv ix val


--  basicUnsafeClear
--  basicUnsafeSet
--  basicUnsafeCopy
--  basicUnsafeMove
--  basicUnsafeGrow
--  basicUnsafeReplicate

  {-#INLINE basicLength#-}
  {-#INLINE basicUnsafeSlice#-}
  {-#INLINE basicOverlaps#-}
  {-#INLINE basicUnsafeNew#-}
  {-#INLINE basicUnsafeRead#-}
  {-#INLINE basicUnsafeWrite#-}

--  {-# INLINE basicUnsafeClear#-}
--  {-# INLINE basicUnsafeSet#-}
--  {-# INLINE basicUnsafeCopy#-}
--  {-# INLINE basicUnsafeMove#-}
--  {-# INLINE basicUnsafeGrow#-}
--  {-# INLINE basicUnsafeReplicate#-}

instance (VGM.MVector UV.MVector elem) => VGM.MVector (BufferMut Unboxed)  elem where
  basicLength = \(UnboxedBufferMut v) -> VGM.basicLength v

  basicUnsafeSlice =
    \ ix1 ix2 (UnboxedBufferMut bv) ->
      UnboxedBufferMut $ VGM.basicUnsafeSlice ix1 ix2 bv

  basicOverlaps =
    \ (UnboxedBufferMut bv1) (UnboxedBufferMut bv2) -> VGM.basicOverlaps bv1 bv2

  basicUnsafeNew = \ size ->
      do
        res<- VGM.basicUnsafeNew size
        return  (UnboxedBufferMut res)

  basicUnsafeRead= \(UnboxedBufferMut bv) ix -> VGM.basicUnsafeRead bv ix

  basicUnsafeWrite = \(UnboxedBufferMut bv ) ix val -> VGM.basicUnsafeWrite bv ix val

--  basicUnsafeClear
--  basicUnsafeSet
--  basicUnsafeCopy
--  basicUnsafeMove
--  basicUnsafeGrow
--  basicUnsafeReplicate


  {-#INLINE basicLength#-}
  {-#INLINE basicUnsafeSlice#-}
  {-#INLINE basicOverlaps#-}
  {-#INLINE basicUnsafeNew#-}
  {-#INLINE basicUnsafeRead#-}
  {-#INLINE basicUnsafeWrite#-}
--  {-# INLINE basicUnsafeClear#-}
--  {-# INLINE basicUnsafeSet#-}
--  {-# INLINE basicUnsafeCopy#-}
--  {-# INLINE basicUnsafeMove#-}
--  {-# INLINE basicUnsafeGrow#-}
--  {-# INLINE basicUnsafeReplicate#-}

\end{code}
