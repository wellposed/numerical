
{-# LANGUAGE TypeFamilies,FlexibleInstances,MultiParamTypeClasses,FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances,StandaloneDeriving,  DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveGeneric #-}
{-# LANGUAGE CPP #-}
module Numerical.Array.Storage(
  Boxed
  ,Unboxed
  ,Stored
  ,BufferPure(..)
  ,BufferMut(..)
  ,Buffer
  ,MBuffer
  ,unsafeBufferThaw
  ,unsafeBufferFreeze) where


import Control.Monad.Primitive ( PrimMonad, PrimState )

import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector as BV
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Unboxed as UV

--import qualified Data.Functor as F hiding (Functor)
--import qualified Data.Foldable as F hiding (Foldable)
--import qualified Data.Traversable  as T hiding (Traversable)
#if  defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 709
--import Data.Functor (Functor)
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
#endif

import Data.Typeable
import Data.Data
import GHC.Generics


{-
FIXME : should i require that the element type and
mode are both instance of Typeable for Buffers?

-}


{-
FIX MEEEEE REMINDERS
make the allocators for   Storable Buffers  do AVX sized alignment
-}

-- | The class instance @'Buffer' mode a@ is a shorthand for saying that a given buffer representation @mode@
-- has a 'VG.Vector' instance for both 'BufferPure'  and  'BufferMut'.
class (VG.Vector (BufferPure mode) a, VGM.MVector (BufferMut mode) a)=> Buffer mode a

instance (VG.Vector (BufferPure mode) a, VGM.MVector (BufferMut mode) a)=> Buffer mode a

-- not sure if MBuffer class should exist, fixme. if/when removed, this
class VGM.MVector (BufferMut mode) a=> MBuffer mode a

-- not sure if MBuffer should exist, FIXME
instance VGM.MVector (BufferMut mode) a=> MBuffer mode a

-- | 'Boxed' is the type index for `Buffer`s that use the  boxed data structure `Data.Vector.Vector`
-- as the underlying storage representation.
data Boxed
  deriving Typeable

deriving instance Data Boxed

-- | 'Unboxed' is the type index for 'Buffer's that use the unboxed data structure
-- 'Data.Vector.Unboxed.Vector' as the underlying storage representation.
data Unboxed
  deriving Typeable

deriving instance Data Unboxed

-- | 'Stored' is the type index for 'Buffer's that use the 'Foreign.Storable'
-- for values, in pinned byte array  buffers, provided by 'Data.Vector.Storable'
data Stored
  deriving Typeable

deriving instance Data Stored

type instance VG.Mutable (BufferPure sort) = BufferMut sort


data family   BufferPure sort  elem

deriving instance Typeable BufferPure

newtype instance BufferPure Boxed elem = BoxedBuffer (BV.Vector elem)
  deriving (Show,Data,Generic,Functor,Foldable,Traversable)



newtype instance BufferPure Unboxed elem = UnboxedBuffer (UV.Vector elem)
  deriving (Show,Data,Generic)
--deriving instance Typeable a => Typeable (BufferPure Unboxed a)

newtype instance BufferPure Stored elem = StorableBuffer (SV.Vector elem)
  deriving (Show,Data,Generic)

data family   BufferMut sort st elem
deriving instance Typeable BufferMut


newtype instance BufferMut Boxed st   elem = BoxedBufferMut (BV.MVector st elem)
  --deriving (Show,Data,Generic)
newtype instance BufferMut Unboxed st elem = UnboxedBufferMut (UV.MVector st elem)
  --deriving (Show,Data,Generic)
newtype instance BufferMut Stored st  elem = StorableBufferMut (SV.MVector st elem)

-- | 'unsafeBufferFreeze'
unsafeBufferFreeze :: (Buffer rep a,PrimMonad m) => BufferMut rep (PrimState m )  a -> m (BufferPure rep a)
unsafeBufferFreeze =  VG.basicUnsafeFreeze

unsafeBufferThaw :: (Buffer rep a,PrimMonad m) => (BufferPure rep a) -> m (BufferMut rep (PrimState m )  a)
unsafeBufferThaw = VG.basicUnsafeThaw

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

  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}

--  {-# INLINE basicUnsafeClear#-}
--  {-# INLINE basicUnsafeSet#-}
--  {-# INLINE basicUnsafeCopy#-}
--  {-# INLINE basicUnsafeMove#-}
--  {-# INLINE basicUnsafeGrow#-}
--  {-# INLINE basicUnsafeReplicate#-}

instance (SV.Storable elem) => VGM.MVector (BufferMut Stored)  elem where
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

  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}


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


  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}

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


  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq  #-}


instance (SV.Storable a)  => VG.Vector (BufferPure Stored) a   where

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


  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq  #-}


instance VG.Vector UV.Vector  a  => VG.Vector (BufferPure Unboxed) a   where

  basicUnsafeFreeze = \(UnboxedBufferMut mv) -> (\x -> return $ UnboxedBuffer x) =<<  VG.basicUnsafeFreeze mv
  basicUnsafeThaw= \(UnboxedBuffer v) ->(\x -> return $  UnboxedBufferMut x) =<< VG.basicUnsafeThaw v
  basicLength = \(UnboxedBuffer v) -> VG.basicLength v
  basicUnsafeSlice =
    \ start len (UnboxedBuffer v) ->  UnboxedBuffer $! VG.basicUnsafeSlice start len v
  basicUnsafeIndexM =
    \ (UnboxedBuffer v) ix  -> VG.basicUnsafeIndexM v ix
  elemseq = \ (UnboxedBuffer v) a b -> VG.elemseq v a b


  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq  #-}
