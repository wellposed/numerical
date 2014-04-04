%
\begin{code}
module Numerical.Array.Generic.Mutable(MArray(..)) where

import Control.Monad.Primitive ( PrimMonad, PrimState )
import Numerical.Array.Layout
import Numerical.Array.Shape 
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


\begin{code}


type family RepConstraint world  rep el :: Constraint
--type instance MArrayElem

data family MArray world rep lay (view::Locality) st rank el

type family  MArrayLocality marr :: Locality where
    MArrayLocality (MArray world rep lay (view::Locality) st rank el) = view 

type family  MArrayLayout marr where 
    MArrayLayout (MArray world rep lay (view::Locality) st rank el)  = lay 

type family MarrayRep marr where
    MArrayRep (MArray world rep lay (view::Locality) st rank el) = rep 


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

    basicIndexToAddress :: ma s  rank a -> Shape rank Int -> Address 
    basicAddressToIndex :: ma s  rank a -> Address -> Shape rank Int 

    -- return the least and greatest valid logical addresses
    basicSmallestAddress :: ma s rank a -> m Address 
    basicGreatestAddress :: ma s rank a -> m Address 

    -- return the least and greatest valid array index
    basicSmallestIndex :: ma s rank a -> m (Shape rank Int)
    basicGreatestIndex :: ma s rank a -> m (Shape rank Int)

    basicNextAddress :: ma s rank a -> Address -> m Address 

    basicNextIndex :: ma s rank a -> (Shape rank Int) -> m (Shape rank Int )


    basicShape :: ma s   rank a -> Shape rank Int 

    basicOverlaps :: ma s  rank el -> ma s  rank a -> Bool 
  
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
