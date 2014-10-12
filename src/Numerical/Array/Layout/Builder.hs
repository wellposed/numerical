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
{-# LANGUAGE NoImplicitPrelude #-}

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

import Numerical.Data.Vector.Pair

import Numerical.Array.Layout.Sparse

import Data.Vector.Algorithms.Intro as IntroSort
import Data.List (group)

import Numerical.InternalUtils

import Prelude hiding (error)


data BatchInit   v = BatchInit    { batchInitSize :: !Int
             ,batchInitKV :: !(Either [v]  (IntFun v))    }
            deriving (Typeable)

materializeBatchMV :: (PrimMonad m, VGM.MVector mv a)  => BatchInit a  -> m (mv (PrimState m) a)
materializeBatchMV  (BatchInit size (Left ls )) =
         do
            newMV <- VGM.new size
            _ <- Prelude.mapM (\(ix ,val )-> VGM.unsafeWrite newMV  ix val ) (zip [0..] $ take size ls)
            return newMV
materializeBatchMV  (BatchInit size (Right (IntFun f) )) =
         do
            newMV <- VGM.new size
            _ <- Prelude.mapM (\ix -> do v <- (f ix) ; VGM.unsafeWrite newMV  ix  v ) $ take size  [0..]
            return newMV

--- not sure if this is EVER useful
newtype AnyMV mv e = AMV (forall s . mv s e )


instance (Show  a)=> Show (BatchInit a) where
  show (BatchInit size (Left ls) )  | size > 100 =  "(BatchInit " ++show size  ++
                                          "-- only showing the first 100 elements\n"
                                          ++ "(Left "++(show $ take 100 ls ) ++ "))\n"
                                    | otherwise ="(BatchInit " ++show size  ++
                                     " (Left "++(show  ls ) ++ "))\n"
  show (BatchInit size (Right (IntFun f)) ) | size > 100 =  "(BatchInit " ++show size  ++
                                          "-- only showing the first 100 elements\n"
                                          ++ "(Left "++(show $ runST (Prelude.mapM f [0..100]) ) ++ "))\n"
                             | otherwise ="(BatchInit " ++show size
                                          ++ "(Left "++(show $ runST (Prelude.mapM f [0,1..size -1]) ) ++ "))\n"


newtype IntFun a = IntFun  (forall m. (PrimMonad m)=>  Int -> m a )
-- This may change substantially in a future release, but for now
-- acts like
            deriving (Typeable)

instance  Functor IntFun  where
  fmap f (IntFun g) = IntFun (\x->   g x  >>= (\ y -> return (f y))  )
  {-# INLINE fmap #-}

instance Functor BatchInit  where
  {-# INLINE fmap  #-}
  fmap = \f bival ->
              case  bival of
                (BatchInit size (Left ls))->
                       BatchInit size (Left (Prelude.map   f  ls  ))
                (BatchInit size (Right gfun))->
                      BatchInit size (Right  $ fmap  f gfun  )



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


fromListBI :: [a] -> BatchInit a
fromListBI ls = BatchInit (length ls) (Left ls)

fromVectorBI :: VG.Vector v e => v e -> BatchInit e
fromVectorBI v =  BatchInit size
      (Right
        (IntFun $
          \i -> if i >= size
              then error  $ " out of bounds index on IntFun of size: " ++ show i
              else return $ v VG.! i
            ))
  where
    size = VG.length v

fromMVectorBI :: (VGM.MVector mv e ) => AnyMV mv e -> BatchInit e
fromMVectorBI (AMV v) =  BatchInit size
      (Right
        (IntFun $
          \i -> if i >= size
              then error  $ " out of bounds index on IntFun of size: " ++ show i
              else  v `VGM.read` i
            ))
  where
    size = VGM.length v



class Layout form (rank::Nat) => LayoutBuilder form (rank::Nat) | form -> rank where

  buildFormatM :: (store~FormatStorageRep form,Buffer store Int ,Buffer store a,PrimMonad m)=>
         Shape rank Int -> proxy form -> a
         -> Maybe (BatchInit  (Shape rank Int,a))
         ->m (form, BufferMut store (PrimState m) a )


buildFormatPure:: forall store form rank proxy m  a. (LayoutBuilder form (rank::Nat)
  ,store~FormatStorageRep form,Buffer store Int  ,Buffer store  a, Monad m ) =>
     Shape rank Int -> proxy form -> a  -> Maybe (BatchInit  (Shape rank Int,a))
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


{-
the dense instances ignore the builder structure, which does suggest that maybe
there shoudl be a dense builder layout class and a sparse layout class separately
-}

instance LayoutBuilder (Format  Direct Contiguous (S Z) rep) (S Z) where

   buildFormatM (size:* _) _ defaultValue _ =
      do
        buf<-  VGM.replicate size defaultValue
        return (FormatDirectContiguous  size,buf)


-- really wish I didn't have to write the foldable and traversable constraints
-- seems like a code smell?!
instance (F.Foldable (Shape r),T.Traversable (Shape r) ,A.Applicative (Shape r))
  => LayoutBuilder (Format  Row Contiguous r rep) r  where

   buildFormatM ix  _ defaultValue _ =
      do
        buf<-  VGM.replicate (F.foldl' (*) 0   ix) defaultValue
        return (FormatRowContiguous   ix,buf)

instance (F.Foldable (Shape r),T.Traversable (Shape r) ,A.Applicative (Shape r))
  =>  LayoutBuilder (Format  Column Contiguous r rep) r  where

   buildFormatM ix  _ defaultValue _ =
      do
        buf<-  VGM.replicate (F.foldl' (*) 0   ix) defaultValue
        return (FormatColumnContiguous   ix,buf)

isStrictlyMonotonicV ::(VG.Vector v e)=> (e -> e->Ordering)-> v e -> Maybe Int
isStrictlyMonotonicV cmp v = go  0 (VG.length v)
  where
    go !i !len  | i+1 >= len   = Nothing
              |  (v VG.! i) `lt` (v VG.! (i+1))= go (i+1) len
             | otherwise = Just i

    lt a b = case cmp a b  of
                  LT -> True
                  _ -> False


instance (Buffer rep Int)=>LayoutBuilder (Format DirectSparse Contiguous (S Z) rep ) (S Z) where


  buildFormatM (size:* _) _ _ Nothing  = do
      mvI <- VGM.new 0
      vI <- VG.unsafeFreeze mvI
      mvV <- VGM.new 0
      return $!  (FormatDirectSparseContiguous size 0 vI, mvV)

  buildFormatM (size:* _) _ _ (Just builder)= do
    -- need to use let so type inference doesnt totally barf
    mvt@(MVPair (MVLeaf ix) (MVLeaf val)) <- materializeBatchMV $ fmap  ( \((ix:*_),v)-> (ix,v)) builder
    -- if i swap to using this  instead of  ix <- mat.. ; val <- mat..
    --i get CRAZY type errors
    -- could this be a ghc bug?
    --ix <- materializeBatchMV $ fmap fst builtTup
    --val <- materializeBatchMV $ fmap snd builtTup
    _<- IntroSort.sortBy  (\x y -> compare (fst x) (fst y)) mvt
                                                              -- this lets me sort a pair of arrays!
    vIx <- VG.unsafeFreeze ix
    optFail  <- return $ isStrictlyMonotonicV   compare vIx
    --_hoelly
    case optFail of
      Nothing -> return (FormatDirectSparseContiguous size 0 vIx, val)
      Just ixWrong ->  error $ "DirectSparse Index duplication at index "++ show (vIx VG.! ixWrong)


instance (Buffer rep Int) => LayoutBuilder (Format CompressedSparseRow Contiguous (S (S Z)) rep ) (S (S Z)) where

  buildFormatM (x:* y :* _) _   _ Nothing= do
    mvi <-  VGM.new 0
    vi <-  VG.unsafeFreeze  mvi
    mvval <- VGM.new 0
    return $
      (FormatContiguousCompressedSparseRow
              (FormatContiguousCompressedSparseInternal y x  vi vi), mvval )

  buildFormatM (x:* y :* _) proxyFormat  _ (Just builder) = do
    mvtup@(MVPair (MVPair (MVLeaf mvectYs) (MVLeaf mvectXs)) (MVLeaf mvectVals))<-
          materializeBatchMV  $ fmap (\((xix:* yix :* _),val)-> ((yix,xix),val) ) builder
    _ <-  IntroSort.sortBy (\((y1,x1),_) ((y2,x2),_) ->  basicCompareIndex  proxyFormat (x1:*y1 :* Nil) (x2:*y2:* Nil)  )
                  mvtup
    vectXs <- unsafeBufferFreeze mvectXs
    vectYs <- unsafeBufferFreeze mvectYs
    --- predicate check here wrt monotonicity of
    --- compute runlength partial sums of where ys go

    -- need to actually check
    yRunsVect <- return $
          VG.replicate (y+1) (0::Int) VG.//  computeStarts  (computeRunLengths vectYs) 0 y
    --_ <- (error "computeRUnCount") vectYs yRunsMVect
    --yRunsVect <- unsafeBufferFreeze yRunsMVect

    return $
      (FormatContiguousCompressedSparseRow
              (FormatContiguousCompressedSparseInternal y x  vectXs yRunsVect), mvectVals )

  buildFormatM _ _  _ _= error "this is actually impossible"
        --- needed to placate coverage checker

computeRunLengths :: (VG.Vector v e, Eq e)=> v e -> [(e,Int)]
computeRunLengths =  \y ->   fmap   (\x ->(head x,length x)) $ group $ VG.toList y



{-# SPECIALIZE computeStarts :: [(Int,Int)]->Int->Int ->[(Int,Int)] #-}

computeStarts:: (Enum a, Ord a, Num b )=>[(a,b)]-> a -> a -> [(a,b)]
computeStarts [] start end | start <= end  = fmap (\x -> (x ,0)) [start..end]
                          |  otherwise = error "bad start end arguments to computeStarts"
computeStarts ls start end | start <= end  = go start 0 ls
                            | otherwise =  error "bad start end arguments to computeStarts"
  where
    --go :: a ->b->[(a,b)]-> [(a,b)]
    go !posNext preSum [] | posNext <= end = fmap (\x -> (x,preSum)) [posNext .. end]
                      | otherwise  = error "impossible go computeStarts "
    go !posNext !preSum gls@((posAt,atSum):rest)
            | posNext < posAt= (posNext,preSum):  go (succ posNext) preSum gls
            | posNext == posAt = (posNext,preSum) : go (succ posNext) (preSum + atSum) rest
            | otherwise = error "bad position in prefix stream for computeStarts go, literally unpossible "



--computeStarts :: (Eq a, Num a)=> [(a,Int)]->Int -> [(a,Int)]
--computeStarts [] len = map (\x -> (x ,0)) [0..len]
--computeStarts ls len = go 0 0 ls
--   where
--    go preSum place [] |  place > len = []
--                        | place == len = [(place,preSum)]
--                        | otherwise = map
