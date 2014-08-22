{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-#  LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes  #-}
{-# LANGUAGE ScopedTypeVariables#-}

module Numerical.Array.Layout.Builder where

import Control.Monad.Primitive ( PrimMonad, PrimState )
import qualified Data.Vector.Generic as VG
--import qualified Data.Vector.Generic.Mutable as VGM
import Numerical.Array.Layout.Base
--import Numerical.Data.Vector.Pair
import Control.Monad.ST.Safe (runST)
import Data.Typeable

-- this is copied from the Vector 0.11 api,
data Chunk v a = Chunk Int (forall m. (PrimMonad m, VG.Vector v a) => VG.Mutable v (PrimState m) a -> m ())
  deriving (Typeable )

data BatchInit v rank a = BatchInit {batchInitSize :: !Int  -- this should be Word not Int
                                ,batchInitIndices :: !(Chunk v (Shape rank Int))
                                ,batchInitValues:: !(Chunk v a)
                              }
            deriving (Typeable)

{-
ChoiceT from monad lib is tempting
as is one of the ListT done right
Bundle from Vector 0.11 and Stream from 0.10 are both alluring too
-}



class Layout form (rank::Nat) => LayoutBuilder form (rank::Nat) | form -> rank where


  buildFormatM :: (store~FormatStorageRep form,VG.Vector (BufferPure store) Int
      ,VG.Vector (BufferPure store) a,PrimMonad m)=>
         Shape rank Int -> proxy form -> a
         -> (Maybe (BatchInit (BufferPure store) rank a) )
         ->m (form, BufferMut store (PrimState m) a )


buildFormatPure:: forall store form rank proxy m  a. (LayoutBuilder form (rank::Nat),store~FormatStorageRep form,VG.Vector (BufferPure store) Int
      ,VG.Vector (BufferPure store) a, Monad m ) =>
     Shape rank Int -> proxy form -> a
         -> (Maybe (BatchInit (BufferPure store) rank a) )
         ->m (form, BufferPure store  a )
buildFormatPure shape prox defaultValue builder =
  do  res@(!_,!_)<-return $! theComputation
      return res
  where
        theComputation :: (form,BufferPure store   a )
        !theComputation = runST $
            do  (form,buf) <- buildFormatM shape prox defaultValue builder
                pureBuff <- VG.unsafeFreeze buf
                return (form, pureBuff)
{-
this is a funky api for both dense and sparse arrays general builder format.

given the target shape, logical dimensions,a default value (only used for dense arrays)
and the list of manifest values (mostly only used for sparse), build the format
descriptor and the suitably initialized and sized values buffer

this api is only meant for internal use for building new array values


TODO: compare using a catenable priority heap vs just doing fast sorting.
-}
