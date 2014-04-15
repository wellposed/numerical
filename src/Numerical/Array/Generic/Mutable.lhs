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
import Numerical.Nat 
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


-- | MutableInnerContigArray is the "meet" (minimum) of the locality level of marr and InnerContiguous.
-- Thus both Contiguous and InnerContiguous are made InnerContiguous, and Strided stays Strided
type family  MutableInnerContigArray (marr :: * -> Nat -> * -> *) st (rank :: Nat) a 


{-
data instance MArray Native Storable lay  view 

-}
--instance Unbox el =>  mutableArray (MArray Native Unboxed) RowMajor (S(S Z)) el 


{-

Mutable Array Builder will only have contiguous instances 
and only makes sense for dense arrays afaik

BE VERY THOUGHTFUL about what instances you write, or i'll be mad
-}
class MutableArrayBuilder marr rank a where 
    basicUnsafeNew :: PrimMonad m => Shape rank Int -> m (marr (PrimState m)  rank a)
    basicUnsafeReplicate :: PrimMonad m => Shape rank Int -> a -> m (marr (PrimState m) rank a)


{-
Mutable

-}

class MutableRectilinear marr rank a where 




    -- | basicSliceMajorAxis arr (x,y) returns the sub array of the same rank,
    -- with the outermost (ie major axis) dimension of arr restricted to the  
    -- (x,y) is an inclusive interval, MUST satisfy x<y , and be a valid
    -- subinterval of the major axis of arr.
    basicSliceMajorAxis :: PrimMonad m => marr (PrimState m) rank a -> (Int,Int)-> m (marr (PrimState m) rank a)

    --  |  semantically, basicProjectMajorAxis arr ix, is the rank reducing version of what 
    -- basicSliceMajorAxis arr (ix,ix) would mean _if_ the (ix,ix) tuple was a legal major axis slice
    -- there exist Array Formats and Ranks where you will not be able to apply basicProjectMajorAxis 
    -- For now i'm leaving it in this class, because it will not be possible to 
    -- use the "illegal" projections in a manner that will type check.
    -- BUT Perhaps this should be revisited in a subsequent de
    basicProjectMajorAxis :: PrimMonad m => marr (PrimState m) (S rank) a -> Int -> m (arr (PrimState m) rank a )

    basicSlice :: PrimMonad m => marr (PrimState m) rank a -> Shape rank Int -> Shape rank Int  
        -> m (MutableInnerContigArray marr (PrimState m) rank a )


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

    --  | basicManifestIndex checks if a index is present or not
    -- helpful primitive for authoring codes for (un)structure sparse array format
    basicManifestIndex :: PrimMonad m => marr (PrimState m) rank a -> Shape rank Int -> m (Maybe Address)

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

\end{code}


Now lets write down a bunch of really really simple examples!
note that these example do not have the right error handling logic currently


\begin{code}


generalizedMatrixDenseVectorProduct ::  forall m a mvect loc marr. 
                        (MutableArrayBuilder (mvect Direct 'Contiguous) N1 a
                        ,(MutableArray  (mvect Direct 'Contiguous) N1 a)
                        , MutableArray marr N2 a
                        , Num a 
                        , PrimMonad m
                        , MutableArray (mvect Direct loc) N1 a)=> 
    marr (PrimState m) N2 a ->  mvect Direct loc (PrimState m) N1 a -> m (mvect Direct Contiguous  (PrimState m) N1 a )
generalizedMatrixDenseVectorProduct mat vect = do
    -- FIXME : check the dimensions match 
    (x:* y :* Nil )<- return $ basicShape mat 
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



generalizedMatrixOuterProduct :: forall m  a lvect rvect matv . (MutableArray lvect N1 a
        ,MutableArray rvect N1 a
        ,MutableArray matv N2 a
        ,PrimMonad m
        ,Num a) => lvect (PrimState m) N1 a -> rvect (PrimState m) N1 a -> matv (PrimState m)  N2  a -> m () 
generalizedMatrixOuterProduct leftV rigthV matV = do 
        (x:* y :* Nil )<- return $ basicShape matV 
        -- use these x and y to check if shape matches the x and y vectors
        firstIx <- basicSmallestIndex matV  
        lastIx <- basicGreatestIndex matV
        go firstIx lastIx
        return ()  
    where 
        go :: Shape N2 Int -> Shape N2 Int -> m () 
        go = undefined 


    

\end{code}








