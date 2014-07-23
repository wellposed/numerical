
{-# LANGUAGE TypeFamilies,FlexibleInstances,MultiParamTypeClasses,FlexibleContexts #-}
module Numerical.Array.Storage(Boxed
  ,Unboxed
  ,Storable
  ,BufferPure(..)
  ,BufferMut(..)) where

import qualified Data.Vector.Generic as VG
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


type instance VG.Mutable (BufferPure sort) = BufferMut sort


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

  {-Q/todo/check fixme, do these other operations need be provided in a pass through way too?
  or will there be no difference in the derived code perf ? -}
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

  {-#INLINE basicLength#-}
  {-#INLINE basicUnsafeSlice#-}
  {-#INLINE basicOverlaps#-}
  {-#INLINE basicUnsafeNew#-}
  {-#INLINE basicUnsafeRead#-}
  {-#INLINE basicUnsafeWrite#-}


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


  {-#INLINE basicLength#-}
  {-#INLINE basicUnsafeSlice#-}
  {-#INLINE basicOverlaps#-}
  {-#INLINE basicUnsafeNew#-}
  {-#INLINE basicUnsafeRead#-}
  {-#INLINE basicUnsafeWrite#-}

----
----
instance VG.Vector BV.Vector  a  => VG.Vector (BufferPure Boxed) a   where

  basicUnsafeFreeze =
     \(BoxedBufferMut mv) ->(\ x->return $ BoxedBuffer x) =<<  VG.basicUnsafeFreeze mv
  basicUnsafeThaw= \(BoxedBuffer v) ->(\x -> return $ BoxedBufferMut x ) =<< VG.basicUnsafeThaw v
  basicLength = \(BoxedBuffer v) -> VG.basicLength v
  basicUnsafeSlice =
    \ start len (BoxedBuffer v) ->  BoxedBuffer $! VG.basicUnsafeSlice start len v
  basicUnsafeIndexM =
    \ (BoxedBuffer v) ix  -> VG.basicUnsafeIndexM v ix
  elemseq = \ (BoxedBuffer v) a b -> VG.elemseq v a b


  {-# INLINE basicUnsafeFreeze#-}
  {-# INLINE basicUnsafeThaw#-}
  {-# INLINE basicLength#-}
  {-# INLINE basicUnsafeSlice#-}
  {-# INLINE basicUnsafeIndexM#-}
  {-# INLINE elemseq #-}


instance VG.Vector SV.Vector  a  => VG.Vector (BufferPure Storable) a   where

  basicUnsafeFreeze =
     \(StorableBufferMut mv) -> (\x ->return $StorableBuffer x) =<<  VG.basicUnsafeFreeze mv
  basicUnsafeThaw=
    \(StorableBuffer v) -> (\x -> return $ StorableBufferMut x) =<< VG.basicUnsafeThaw v
  basicLength = \(StorableBuffer v) -> VG.basicLength v
  basicUnsafeSlice =
    \ start len (StorableBuffer v) ->  StorableBuffer $! VG.basicUnsafeSlice start len v
  basicUnsafeIndexM =
    \ (StorableBuffer v) ix  -> VG.basicUnsafeIndexM v ix
  elemseq = \ (StorableBuffer v) a b -> VG.elemseq v a b


  {-# INLINE basicUnsafeFreeze#-}
  {-# INLINE basicUnsafeThaw#-}
  {-# INLINE basicLength#-}
  {-# INLINE basicUnsafeSlice#-}
  {-# INLINE basicUnsafeIndexM#-}
  {-# INLINE elemseq #-}


instance VG.Vector UV.Vector  a  => VG.Vector (BufferPure Unboxed) a   where

  basicUnsafeFreeze = \(UnboxedBufferMut mv) -> (\x -> return $ UnboxedBuffer x) =<<  VG.basicUnsafeFreeze mv
  basicUnsafeThaw= \(UnboxedBuffer v) ->(\x -> return $  UnboxedBufferMut x) =<< VG.basicUnsafeThaw v
  basicLength = \(UnboxedBuffer v) -> VG.basicLength v
  basicUnsafeSlice =
    \ start len (UnboxedBuffer v) ->  UnboxedBuffer $! VG.basicUnsafeSlice start len v
  basicUnsafeIndexM =
    \ (UnboxedBuffer v) ix  -> VG.basicUnsafeIndexM v ix
  elemseq = \ (UnboxedBuffer v) a b -> VG.elemseq v a b


  {-# INLINE basicUnsafeFreeze#-}
  {-# INLINE basicUnsafeThaw#-}
  {-# INLINE basicLength#-}
  {-# INLINE basicUnsafeSlice#-}
  {-# INLINE basicUnsafeIndexM#-}
  {-# INLINE elemseq #-}

