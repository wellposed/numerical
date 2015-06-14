
{- | This  module is pretty cool because it gives you a way to talk about
open struct of arrays style vectors

might be replaced with an HList of Vectors approach


-}



{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses ,FlexibleInstances  , FlexibleContexts,UndecidableInstances #-}

module  Numerical.Data.Vector.Pair(
    VProd(..)
    ,vPair
    ,vUnPair
    ,MVProd(..)
    --,mvUnPair
    ,Prod(..)
    --,mvPair
      ) where

import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as MV

import Control.Monad.Primitive (PrimMonad)

--type instance V.Mutable (VPair v) = MVPair (V.Mutable v)


{-
currently primmonad doesn't get its free applicative/functor powers :*(

-}

(<$$$>) :: PrimMonad m => (a->b) -> m a -> m b
(<$$$>) f mv = do v <- mv ; return (f v )
{-# INLINE (<$$$>) #-}

(<***>) :: PrimMonad m => m (a->b) -> m a -> m b
(<***>) mf mv =  do f <- mf ; v <- mv ; return (f v)
{-# INLINE (<***>) #-}

{-
probably should just

-}


data Prod = Pair Prod Prod | Unit


data family   VProd  (vect :: * -> * ) (prd:: Prod ) val  -- where
data instance VProd v 'Unit a where
    VLeaf ::  !(v a) -> VProd v   'Unit a

data instance VProd v ('Pair pra prb )  (a,b) where
    VPair  :: !(VProd v pra a) -> !(VProd v prb b ) ->VProd v ('Pair  pra prb) (a,b)

data family   MVProd  (vect :: * -> * -> * )  (prd:: Prod ) (st :: * ) val  -- where
data instance   MVProd mv 'Unit  st a where
  MVLeaf :: !(mv  st a) -> MVProd mv  'Unit st  a
data instance   MVProd mv ('Pair pra prb)  st (a,b) where
    MVPair  :: !(MVProd mv pra st a) -> !(MVProd mv  prb   st b ) -> MVProd mv  ('Pair pra prb) st (a,b)


vPair :: (v a,v b)->VProd v ('Pair 'Unit 'Unit) (a,b)
vPair  = \ (va,vb) ->  VPair (VLeaf va) (VLeaf vb)
{-# INLINE vPair #-}

vUnPair  :: VProd v ('Pair 'Unit 'Unit) (a,b) -> (v a, v b)
vUnPair = \ (VPair (VLeaf va) (VLeaf vb))-> (va,vb)
{-# INLINE vUnPair #-}

type instance  V.Mutable (VProd vec prod)= MVProd (V.Mutable vec) prod


--mvPair :: (mv st a,mv st b)->MVPair mv st (a,b)
--mvPair  = \ (mva, mvb) ->  TheMVPair mva mvb
--{-# INLINE mvPair #-}

--mvUnPair  :: MVPair mv st  (a,b) -> (mv st a,mv st b)
--mvUnPair = \ (TheMVPair mva mvb)-> (mva,mvb)
--{-# INLINE mvUnPair #-}

instance  (MV.MVector (MVProd (V.Mutable v) ('Pair pa pb )  ) (a,b) ,V.Vector (VProd v pa) a,V.Vector (VProd v pb) b)
  => V.Vector (VProd v ('Pair pa pb )) (a,b)  where
    {-# INLINE  basicUnsafeFreeze #-}
    basicUnsafeFreeze = \(MVPair mva mvb) ->
      VPair <$$$> V.basicUnsafeFreeze mva <***> V.basicUnsafeFreeze mvb

    {-# INLINE basicUnsafeThaw #-}
    basicUnsafeThaw = \(VPair va vb) ->
      MVPair <$$$> V.basicUnsafeThaw va <***> V.basicUnsafeThaw vb

    {-# INLINE basicLength #-}
    basicLength = \(VPair va _) -> V.basicLength va

    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice = \start len (VPair va vb) ->
      VPair (V.basicUnsafeSlice start len va) (V.basicUnsafeSlice start len vb)

    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeIndexM = \(VPair va vb) ix ->
      do
          a <- V.basicUnsafeIndexM va ix
          b <- V.basicUnsafeIndexM vb ix
          return (a,b)

instance  (MV.MVector (MVProd (V.Mutable v) 'Unit  ) a ,V.Vector v a)
  => V.Vector (VProd v 'Unit) a  where

    {-# INLINE  basicUnsafeFreeze #-}
    {-# INLINE basicUnsafeThaw #-}
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicUnsafeIndexM #-}

    basicUnsafeFreeze = \(MVLeaf mva) ->
      VLeaf <$$$> V.basicUnsafeFreeze mva
    basicUnsafeThaw = \(VLeaf va ) ->
      MVLeaf <$$$> V.basicUnsafeThaw va
    basicLength = \(VLeaf va ) -> V.basicLength va
    basicUnsafeSlice = \start len (VLeaf va ) ->
      VLeaf(V.basicUnsafeSlice start len va)
    basicUnsafeIndexM = \(VLeaf va) ix ->  V.basicUnsafeIndexM va ix


instance (MV.MVector mv a) => MV.MVector (MVProd mv 'Unit) a where
  basicLength = \ (MVLeaf mva) -> MV.basicLength mva
  {-# INLINE basicLength #-}

  basicUnsafeSlice = \ start len (MVLeaf mva  )->
    MVLeaf (MV.basicUnsafeSlice start len mva)
  {-# INLINE basicUnsafeSlice #-}

  basicOverlaps = \ (MVLeaf mva ) (MVLeaf mva2 )-> (MV.basicOverlaps mva mva2)
  {-# INLINE basicOverlaps #-}

  basicUnsafeNew =
      \ size ->
          MVLeaf <$$$> MV.basicUnsafeNew size
  {-# INLINE basicUnsafeNew #-}

  basicUnsafeReplicate =
      \ size a ->
         MVLeaf <$$$>
            MV.basicUnsafeReplicate size a
  {-# INLINE basicUnsafeReplicate #-}

  basicUnsafeRead = \(MVLeaf mva ) ix ->   MV.basicUnsafeRead mva ix
  {-#INLINE basicUnsafeRead #-}

  basicUnsafeWrite = \ (MVLeaf mva ) ix a  ->
    do
      MV.basicUnsafeWrite mva ix a
      return ()
  {-#INLINE basicUnsafeWrite #-}

  {-#INLINE basicUnsafeGrow #-}
  basicUnsafeGrow = \ (MVLeaf mva ) growth ->
      MVLeaf <$$$> MV.basicUnsafeGrow mva growth



instance (MV.MVector (MVProd mv pra) a,MV.MVector (MVProd mv prb) b) => MV.MVector (MVProd mv ('Pair pra prb)) (a,b) where
  basicLength = \ (MVPair mva _) -> MV.basicLength mva
  {-# INLINE basicLength #-}

  basicUnsafeSlice = \ start len (MVPair mva mvb )->
    MVPair (MV.basicUnsafeSlice start len mva) (MV.basicUnsafeSlice start len mvb)
  {-# INLINE basicUnsafeSlice #-}

  basicOverlaps = \ (MVPair mva mvb) (MVPair mva2 mvb2)-> (MV.basicOverlaps mva mva2) || (MV.basicOverlaps mvb mvb2)
  {-# INLINE basicOverlaps #-}

  basicUnsafeNew =
      \ size ->
          MVPair <$$$> MV.basicUnsafeNew size <***> MV.basicUnsafeNew size
  {-# INLINE basicUnsafeNew #-}

  basicUnsafeReplicate =
      \ size (a,b) ->
         MVPair <$$$>
            MV.basicUnsafeReplicate size a <***>
            MV.basicUnsafeReplicate size b
  {-# INLINE basicUnsafeReplicate #-}

  basicUnsafeRead = \(MVPair mva mvb) ix ->
    (,) <$$$>  MV.basicUnsafeRead mva ix <***> MV.basicUnsafeRead mvb ix

  {-# INLINE basicUnsafeRead #-}

  basicUnsafeWrite = \ (MVPair mva mvb) ix (a,b) ->
    do
      MV.basicUnsafeWrite mva ix a
      MV.basicUnsafeWrite mvb ix b
      return ()
  {-#INLINE basicUnsafeWrite #-}

  {-#INLINE basicUnsafeGrow #-}
  basicUnsafeGrow = \ (MVPair mva mvb) growth ->
      MVPair <$$$> MV.basicUnsafeGrow mva growth <***>
          MV.basicUnsafeGrow mvb growth





