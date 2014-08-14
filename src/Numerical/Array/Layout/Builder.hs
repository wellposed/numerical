{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-#  LANGUAGE GADTs #-}

module Numerical.Array.Layout.Builder where

import Control.Monad.Primitive ( PrimMonad, PrimState )
--import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import Numerical.Array.Layout.Base


class Layout form (rank::Nat) => LayoutBuilder form (rank::Nat) | form -> rank where
  --this should maybe go somewhere else? as an open type family?
  type LayoutStorage form :: *

  buildFormatM :: (VGM.MVector (BufferMut (LayoutStorage form)) Int
      ,VGM.MVector (BufferMut (LayoutStorage form)) a,PrimMonad m)=>
         Shape rank Int -> proxy form -> a ->   (Maybe [(Shape rank Int,a)])->m (form, BufferMut (LayoutStorage form) (PrimState m) a )


{-
this is a funky api for both dense and sparse arrays general builder format.

given the target shape, logical dimensions,a default value (only used for dense arrays)
and the list of manifest values (mostly only used for sparse), build the format
descriptor and the suitably initialized and sized values buffer

this api is only meant for internal use for building new array values


TODO: compare using a catenable priority heap vs just doing fast sorting.
-}
