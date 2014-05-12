\begin{code}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE MultiParamTypeClasses  #-}

module  Numerical.Internal.Data.Vector(VPair(..),MVPair(..)) where

import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as MV

data family VPair (vect :: * -> * ) val
data instance VPair v (a,b)= TheVPair !(v a) !(v b)

data family MVPair (vect :: * -> * -> *) st val
data instance MVPair mv st (a,b) = TheMVPair !(mv st a) !(mv st b)

instance (MV.MVector mv a,MV.MVector mv b) => MV.MVector (MVPair mv ) (a,b) where
  basicLength = \ (TheMVPair mva mvb) -> MV.basicLength mva
  {-# INLINE basicLength #-}

  basicUnsafeSlice = \ start len (TheMVPair mva mvb )->
    TheMVPair (MV.basicUnsafeSlice start len mva) (MV.basicUnsafeSlice start len mvb)
  {-# INLINE basicUnsafeSlice#-}

  basicOverlaps = \ (TheMVPair mva mvb) (TheMVPair mva2 mvb2)-> (MV.basicOverlaps mva mva2) || (MV.basicOverlaps mvb mvb2)
  {-# INLINE basicOverlaps #-}

  basicUnsafeNew =
      \ size ->
          do  mva <- MV.basicUnsafeNew size ;
              mvb <- MV.basicUnsafeNew size ;
              return  (TheMVPair mva mvb)
  {-# INLINE basicUnsafeNew #-}

  basicUnsafeReplicate =
      \ size (a,b) ->
        do  mva <- MV.basicUnsafeReplicate size a
            mvb <- MV.basicUnsafeReplicate size b
            return (TheMVPair mva mvb)
  {-# INLINE basicUnsafeReplicate #-}

  basicUnsafeRead = \(TheMVPair mva mvb) ix ->
    do
        a <- MV.basicUnsafeRead mva ix
        b <- MV.basicUnsafeRead mvb ix
        return (a,b)
  {-#INLINE basicUnsafeRead#-}

  basicUnsafeWrite = \ (TheMVPair mva mvb) ix (a,b) ->
    do
      MV.basicUnsafeWrite mva ix a
      MV.basicUnsafeWrite mvb ix b
      return ()
  {-#INLINE basicUnsafeWrite#-}








\end{code}
