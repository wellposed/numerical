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
-- {-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Numerical.Array.Layout.Builder where

import Control.Monad.Primitive ( PrimMonad, PrimState )
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import Numerical.Array.Layout.Base
import  Numerical.Array.Layout.Dense as Dense
--import Numerical.Array.Layou.Sparse as Sparse
--import Numerical.Data.Vector.Pair
import Control.Monad.ST.Safe (runST)
import Data.Typeable
import qualified  Data.Foldable as F
import   Data.Traversable as T
import   Control.Applicative as A



data BatchInit  rank a = BatchInit    { batchInitSize :: !Int
             ,batchInitKV :: !(Either [(Shape rank Int,a)]  (IntFun (Shape rank Int,a)))    }
            deriving (Typeable)




instance (Show  a , Show  (Shape rank Int))=> Show (BatchInit rank a) where
  show (BatchInit size (Left ls) )  | size > 100 =  "(BatchInit " ++show size  ++
                                          "-- only showing the first 100 elements\n"
                                          ++ "(Left "++(show $ take 100 ls ) ++ "))\n"
                                    | otherwise ="(BatchInit " ++show size  ++
                                     " (Left "++(show  ls ) ++ "))\n"
  show (BatchInit size (Right (Ifun f)) ) | size > 100 =  "(BatchInit " ++show size  ++
                                          "-- only showing the first 100 elements\n"
                                          ++ "(Left "++(show $ runST (Prelude.mapM f [0..100]) ) ++ "))\n"
                             | otherwise ="(BatchInit " ++show size
                                          ++ "(Left "++(show $ runST (Prelude.mapM f [0,1..size -1]) ) ++ "))\n"


newtype IntFun a = Ifun  (forall m. (PrimMonad m,Functor m )=>  Int -> m a )
-- This may change substantially in a future release, but for now
-- acts like
            deriving (Typeable)

instance  Functor IntFun  where
  fmap f (Ifun g) = Ifun (\x->  f <$> g x  )
  {-# INLINE fmap #-}

instance Functor (BatchInit  rank) where
  {-# INLINE fmap  #-}
  fmap = \f bival ->
              case  bival of
                (BatchInit size (Left ls))->
                       BatchInit size (Left (Prelude.map   (\(ix,val)-> (ix, f val)) ls  ))
                (BatchInit size (Right gfun))->
                      BatchInit size (Right  $ fmap  (\(ix,val)-> (ix, f val)) gfun  )



-- batchInit size should be Word rather than Int for size, but Vector is lame like that


{-
ChoiceT from monad lib is tempting
as is one of the ListT done right
Bundle from Vector 0.11 and Stream from 0.10 are both alluring too

but all of them make things complicated,
punt for now


ALso: I may want/need to distinguish sparse vs dense builders
and put them into different classes, punting that for now
-}



class Layout form (rank::Nat) => LayoutBuilder form (rank::Nat) | form -> rank where

  buildFormatM :: (store~FormatStorageRep form,VG.Vector (BufferPure store) Int
      ,VG.Vector (BufferPure store) a,PrimMonad m)=>
         Shape rank Int -> proxy form -> a
         -> Maybe (BatchInit  rank a)
         ->m (form, BufferMut store (PrimState m) a )


buildFormatPure:: forall store form rank proxy m  a.
  (LayoutBuilder form (rank::Nat),store~FormatStorageRep form,VG.Vector (BufferPure store) Int  ,VG.Vector (BufferPure store) a, Monad m ) =>
     Shape rank Int -> proxy form -> a  -> Maybe (BatchInit  rank a)   ->m (form, BufferPure store  a )
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


{-
the dense instances ignore the builder structure, which does suggest that maybe
there shoudl be a dense builder layout class and a sparse layout class separately
-}

instance (VG.Vector (BufferPure rep) Int)=> LayoutBuilder (Format  Direct Contiguous (S Z) rep) (S Z) where

   buildFormatM (size:* _) _ defaultValue _ =
      do
        buf<-  VGM.replicate size defaultValue
        return (FormatDirectContiguous  size,buf)


-- really wish I didn't have to write the foldable and traversable constraints
-- seems like a code smell?!
instance (VG.Vector (BufferPure rep) Int,F.Foldable (Shape r),T.Traversable (Shape r) ,A.Applicative (Shape r))=> LayoutBuilder (Format  Row Contiguous r rep) r  where

   buildFormatM ix  _ defaultValue _ =
      do
        buf<-  VGM.replicate (F.foldl' (*) 0   ix) defaultValue
        return (FormatRowContiguous   ix,buf)

instance (VG.Vector (BufferPure rep) Int,F.Foldable (Shape r),T.Traversable (Shape r) ,A.Applicative (Shape r))=>  LayoutBuilder (Format  Column Contiguous r rep) r  where

   buildFormatM ix  _ defaultValue _ =
      do
        buf<-  VGM.replicate (F.foldl' (*) 0   ix) defaultValue
        return (FormatColumnContiguous   ix,buf)
