%
\begin{code}
-- {-# LANGUAGE PolyKinds   #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables#-}
-- {-#  LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-#  LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-#  LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}


module Numerical.Array.Generic.Mutable(MArray(..)) where

import Control.Monad.Primitive ( PrimMonad, PrimState )
import qualified Numerical.Array.DenseLayout as L 
import Numerical.Array.DenseLayout (Address(..),Locality(..),Direct(..))
import Numerical.Array.Shape 
import GHC.Prim(Constraint)

import qualified Data.Vector.Storable.Mutable as SM
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.Vector.Mutable as BM
\end{code}
--
-- For now we're going to just crib the vector style api and Lift it
-- up into a multi dimensional setting.
--
-- the tentative design is to have something like



you'd think that the following array type is ``right''
but then you'll hit problems supporting 
\begin{verbatim}

data MArray world rep lay (view:: Locality) rank elm where
     MArray
         {_marrBuffer :: {-# UNPACK #-}!(MBuffer  world rep elm)
         ,_marrForm :: {-# UNPACK #-} !(Form lay loc rank) 
         --,_marrShift :: {-# UNPACK #-} !Address 
         }
\end{verbatim}

shift will be zero for most reps i'll ever care about, but in certain cases,
might not be. So for now not including it, but might be needed later,
though likely in regards to some sparse format of some sort.

Omitting it for now, but may need to revisit later!

For now any ``Address'' shift will need to be via the buffer 



we need to have ``RepConstraint'' be decoupled from the type classe instances
because we to sometimes have things that are world parametric

indexing should be oblivious to locality,



set and copy and move  require "structured" matrices, where 
 clear really 

\begin{verbatim}
    -- | Set all elements of the vector to the given value. This method should
    -- not be called directly, use 'set' instead.
    basicSet         :: PrimMonad m => marr (PrimState m)  rank  a -> a -> m ()

    -- | Copy a vector. The two vectors may not overlap. This method should not
    -- be called directly, use 'unsafeCopy' instead.
    basicUnsafeCopy  :: PrimMonad m => marr (PrimState m)  rank a   -- ^ target
                              -> marr (PrimState m)  rank a   -- ^ source
                              -> m ()

    -- | Move the contents of a vector. The two vectors may overlap. This method
    -- should not be called directly, use 'unsafeMove' instead.
    basicUnsafeMove  :: PrimMonad m => marr (PrimState m) a   -- ^ target
                              -> v (PrimState m) a   -- ^ source
                              -> m ()



    
\end{verbatim}


NB: one important assumption we'll have for now, is that every 


\begin{code}


type family RepConstraint world  rep el :: Constraint
--type instance MArrayElem

data family MArray world rep lay (view::Locality) st rank el

data NativeWorld 


#if defined(__GLASGOW_HASKELL_) && __GLASGOW_HASKELL__ >= 707
type family  MArrayLocality marr :: Locality where
    MArrayLocality (MArray world rep lay (view::Locality) st rank el) = view 

type family  MArrayLayout marr where 
    MArrayLayout (MArray world rep lay (view::Locality) st rank el)  = lay 

type family MArrayRep marr where
    MArrayRep (MArray world rep lay (view::Locality) st rank el) = rep 
#else 
type family  MArrayLocality marr :: Locality 
type instance     MArrayLocality (MArray world rep lay (view::Locality) st rank el) = view 

type family  MArrayLayout marr  
type instance     MArrayLayout (MArray world rep lay (view::Locality) st rank el)  = lay 

type family MArrayRep marr 
type instance    MArrayRep (MArray world rep lay (view::Locality) st rank el) = rep     

#endif



{-
data instance MArray Native Storable lay  view 

-}
--instance Unbox el =>  mutableArray (MArray Native Unboxed) RowMajor (S(S Z)) el 


{-

Mutable Array Builder will only have contiguous instances 
and only makes sense for dense arrays afaik
-}
class MutableArrayBuilder marr rank a where 
    basicUnsafeNew :: PrimMonad m => Shape rank Int -> m (marr (PrimState m)  rank a)
    basicUnsafeReplicate :: PrimMonad m => Shape rank Int -> a -> m (marr (PrimState m) rank a)




class  MutableArray marr   rank   a  where
    --data  MBuffer world rep lay

    basicIndexToAddress ::  marr s  rank a -> Shape rank Int -> Address 


    basicAddressToIndex :: marr s  rank a -> Address -> Shape rank Int 

    -- return the least valid logical addresses
    basicSmallestAddress :: marr (PrimState m) rank a -> m Address 

    basicGreatestAddress :: marr (PrimState m ) rank a -> m Address 

    -- |  return the smallest valid array index
    --  should be weakly dominated by every other valid index
    basicSmallestIndex :: PrimMonad m => marr (PrimState m) rank a -> m (Shape rank Int)

    -- | return the greatest valid array index
    -- should weakly dominate every 
    basicGreatestIndex ::PrimMonad m =>  marr (PrimState m) rank a -> m (Shape rank Int)

    -- | gives the next valid logical address 
    -- undefined on invalid addresses and the greatest valid address.
    -- Note that for invalid addresses in between minAddress and maxAddress,
    -- will return the next valid address 
    basicNextAddress :: PrimMonad m => marr (PrimState m) rank a -> Address -> m Address 

    -- | gives the next valid array index
    -- undefined on invalid indices and the greatest valid index 
    basicNextIndex ::PrimMonad m =>  marr (PrimState m) rank a -> (Shape rank Int) -> m (Shape rank Int )

    -- | gives the shape, a 'rank' length list of the dimensions
    basicShape :: marr st   rank a -> Shape rank Int 


    basicOverlaps :: marr st  rank a -> marr st  rank a -> Bool 

    -- | 
    basicUnsafeAddressRead  :: PrimMonad m => marr  (PrimState m)  rank a -> Address-> m a

    -- | 
    basicUnsafeAddressWrite :: PrimMonad m => marr  (PrimState m)  rank a -> Address -> a -> m ()
  
    -- | Yield the element at the given position. This method should not be
    -- called directly, use 'unsafeRead' instead.
    basicUnsafeRead  :: PrimMonad m => marr  (PrimState m)  rank a -> Shape rank Int -> m a

    -- | Replace the element at the given position. This method should not be
    -- called directly, use 'unsafeWrite' instead.
    basicUnsafeWrite :: PrimMonad m => marr (PrimState m)  rank a -> Shape rank Int  -> a -> m ()

    -- | Reset all elements of the vector to some undefined value, clearing all
    -- references to external objects. This is usually a noop for unboxed
    -- vectors. This method should not be called directly, use 'clear' instead.
    basicClear       :: PrimMonad m => marr (PrimState m) rank  a -> m ()

--instance MutableArrayBuilder  (MArray NativeWorld ) where
--    func = 




generalizedMatrixDenseVectorProduct ::  forall m a mvect loc marr. 
                        (MutableArrayBuilder (mvect Direct 'Contiguous) ('S 'Z) a
                        ,(MutableArray  (mvect Direct 'Contiguous) ('S 'Z) a)
                        , MutableArray marr (S(S Z)) a
                        , Num a 
                        , PrimMonad m
                        , MutableArray (mvect Direct loc) (S Z) a)=> 
    marr (PrimState m) (S(S Z)) a ->  mvect Direct loc (PrimState m) (S Z) a -> m (mvect Direct Contiguous  (PrimState m) (S Z) a )
generalizedMatrixDenseVectorProduct mat vect = do
    (x:* y :* Nil )<- return $! basicShape mat 
    resultVector <- basicUnsafeReplicate (y:* Nil ) 0  
    firstIx <- basicSmallestIndex mat  
    lastIx <- basicGreatestIndex mat 
    go  firstIx  lastIx resultVector
    return resultVector
    where 
        go ::(MutableArray (mvect Direct Contiguous) (S Z) a) =>  
            Shape (S (S Z)) Int -> Shape (S (S Z)) Int -> 
                (mvect Direct Contiguous  (PrimState m) (S Z) a )->m ()
        go ix@(ix_x :*ix_y :* Nil) last resVector 
            |  last == ix = do   
                    matval <- basicUnsafeRead mat ix 
                    inVectval <- basicUnsafeRead vect (ix_x :* Nil)
                    resVectVal <- basicUnsafeRead resVector (ix_y :* Nil)
                    basicUnsafeWrite resVector (ix_y :* Nil) (resVectVal + (inVectval * matval))
                    return () 


            | last `weaklyDominates` ix = do   
                    matval <- basicUnsafeRead mat ix 
                    inVectval <- basicUnsafeRead vect (ix_x :* Nil)
                    resVectVal <- basicUnsafeRead resVector (ix_y :* Nil)
                    basicUnsafeWrite resVector (ix_y :* Nil) (resVectVal + (inVectval * matval))
                    nextIx  <- basicNextIndex  mat ix 
                    go nextIx last resVector  
            | otherwise = error "impossible thingy with matrixvector product, send help"




    --(x,y)  resV_y +=  m_(x,y) * v_x  

\end{code}








\begin{verbatim}
 #include "vector.h"

-- | Class of mutable vectors parametrised with a primitive state token.
--
class MVector v a where
  -- | Length of the mutable vector. This method should not be
  -- called directly, use 'length' instead.
  basicLength       :: v s a -> Int

  -- | Yield a part of the mutable vector without copying it. This method
  -- should not be called directly, use 'unsafeSlice' instead.
  basicUnsafeSlice :: Int  -- ^ starting index
                   -> Int  -- ^ length of the slice
                   -> v s a
                   -> v s a

  -- Check whether two vectors overlap. This method should not be
  -- called directly, use 'overlaps' instead.
  basicOverlaps    :: v s a -> v s a -> Bool

  -- | Create a mutable vector of the given length. This method should not be
  -- called directly, use 'unsafeNew' instead.
  basicUnsafeNew   :: PrimMonad m => Int -> m (v (PrimState m) a)

  -- | Create a mutable vector of the given length and fill it with an
  -- initial value. This method should not be called directly, use
  -- 'replicate' instead.
  basicUnsafeReplicate :: PrimMonad m => Int -> a -> m (v (PrimState m) a)

  -- | Yield the element at the given position. This method should not be
  -- called directly, use 'unsafeRead' instead.
  basicUnsafeRead  :: PrimMonad m => v (PrimState m) a -> Int -> m a

  -- | Replace the element at the given position. This method should not be
  -- called directly, use 'unsafeWrite' instead.
  basicUnsafeWrite :: PrimMonad m => v (PrimState m) a -> Int -> a -> m ()

  -- | Reset all elements of the vector to some undefined value, clearing all
  -- references to external objects. This is usually a noop for unboxed
  -- vectors. This method should not be called directly, use 'clear' instead.
  basicClear       :: PrimMonad m => v (PrimState m) a -> m ()

  -- | Set all elements of the vector to the given value. This method should
  -- not be called directly, use 'set' instead.
  basicSet         :: PrimMonad m => v (PrimState m) a -> a -> m ()

  -- | Copy a vector. The two vectors may not overlap. This method should not
  -- be called directly, use 'unsafeCopy' instead.
  basicUnsafeCopy  :: PrimMonad m => v (PrimState m) a   -- ^ target
                                  -> v (PrimState m) a   -- ^ source
                                  -> m ()

  -- | Move the contents of a vector. The two vectors may overlap. This method
  -- should not be called directly, use 'unsafeMove' instead.
  basicUnsafeMove  :: PrimMonad m => v (PrimState m) a   -- ^ target
                                  -> v (PrimState m) a   -- ^ source
                                  -> m ()

  -- | Grow a vector by the given number of elements. This method should not be
  -- called directly, use 'unsafeGrow' instead.
  basicUnsafeGrow  :: PrimMonad m => v (PrimState m) a -> Int
                                                       -> m (v (PrimState m) a)

  {-# INLINE basicUnsafeReplicate #-}
  basicUnsafeReplicate n x
    = do
        v <- basicUnsafeNew n
        basicSet v x
        return v

  {-# INLINE basicClear #-}
  basicClear _ = return ()

  {-# INLINE basicSet #-}
  basicSet !v x
    | n == 0    = return ()
    | otherwise = do
                    basicUnsafeWrite v 0 x
                    do_set 1
    where
      !n = basicLength v

      do_set i | 2*i < n = do basicUnsafeCopy (basicUnsafeSlice i i v)
                                              (basicUnsafeSlice 0 i v)
                              do_set (2*i)
               | otherwise = basicUnsafeCopy (basicUnsafeSlice i (n-i) v)
                                             (basicUnsafeSlice 0 (n-i) v)

  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy !dst !src = do_copy 0
    where
      !n = basicLength src

      do_copy i | i < n = do
                            x <- basicUnsafeRead src i
                            basicUnsafeWrite dst i x
                            do_copy (i+1)
                | otherwise = return ()

  {-# INLINE basicUnsafeMove #-}
  basicUnsafeMove !dst !src
    | basicOverlaps dst src = do
        srcCopy <- basicUnsafeNew (basicLength src)
        basicUnsafeCopy srcCopy src
        basicUnsafeCopy dst srcCopy
    | otherwise = basicUnsafeCopy dst src

  {-# INLINE basicUnsafeGrow #-}
  basicUnsafeGrow v by
    = do
        v' <- basicUnsafeNew (n+by)
        basicUnsafeCopy (basicUnsafeSlice 0 n v') v
        return v'
    where
      n = basicLength v
\end{verbatim}
