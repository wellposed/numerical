{- | This  module is pretty cool because it gives you a way to talk about
heterogeneous representations for different columns!

might be replaced with an HList of Vectors approach
-}


{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses ,FlexibleInstances  , FlexibleContexts,UndecidableInstances #-}

module  Numerical.Data.Vector.HPair(
    VHProd(..)
    ,vHPair
    ,vUnHPair
    ,MVHProd(..)
    ,HProd(..)
    ,MutableHProdTree
    ,TransformHProdTree
    --,mvUnPair
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

{-
the names are terrible, fix them later!
HProd , HPair, HUnit, VHPro
-}


data HProd a  where
    HPair :: HProd a-> HProd a  -> HProd a
    HUnit :: a -> HProd a

data  VHProd  (prd:: HProd ( * -> * )) val where
    VHLeaf ::  !(v a) -> VHProd   ('HUnit v) a
    VHNode  :: !(VHProd  pra a) -> !(VHProd  prb b ) ->VHProd  ('HPair  pra prb) (a,b)

data  MVHProd   (prd:: HProd (* -> * -> *) ) (st :: * ) val where
    MVHLeaf :: !(mv  st a) -> MVHProd   ('HUnit mv) st  a
    MVHNode  :: !(MVHProd pra st a) -> !(MVHProd   prb   st b ) -> MVHProd  ('HPair pra prb) st (a,b)


vHPair :: (va a,vb b)->VHProd ('HPair ('HUnit va) ('HUnit vb)) (a,b)
vHPair  = \ (va,vb) ->  VHNode (VHLeaf va) (VHLeaf vb)
{-# INLINE vHPair #-}

vUnHPair  :: VHProd  ('HPair ('HUnit va) ('HUnit vb)) (a,b) -> (va a, vb b)
vUnHPair = \ (VHNode (VHLeaf va) (VHLeaf vb))-> (va,vb)
{-# INLINE vUnHPair #-}

type instance  V.Mutable (VHProd  prod)= MVHProd  (MutableHProdTree prod)

type family MutableHProdTree (a :: HProd (* -> *)) :: HProd (* -> * -> * )  where
  MutableHProdTree ('HUnit v ) = 'HUnit (V.Mutable v)
  MutableHProdTree ('HPair left right) = 'HPair (MutableHProdTree left) (MutableHProdTree right )

type family TransformHProdTree (f :: k-> m) (a :: HProd k) :: HProd m where
  TransformHProdTree f ('HUnit v)= 'HUnit (f v)
  TransformHProdTree f ('HPair left right) = 'HPair (TransformHProdTree f left) (TransformHProdTree f right)




--mvPair :: (mv st a,mv st b)->MVPair mv st (a,b)
--mvPair  = \ (mva, mvb) ->  TheMVPair mva mvb
--{-# INLINE mvPair #-}

--mvUnPair  :: MVPair mv st  (a,b) -> (mv st a,mv st b)
--mvUnPair = \ (TheMVPair mva mvb)-> (mva,mvb)
--{-# INLINE mvUnPair #-}

instance  (MV.MVector (MVHProd  (MutableHProdTree ('HPair pa pb )) ) (a,b) ,
  V.Vector (VHProd  pa) a, V.Vector (VHProd  pb) b)
  => V.Vector (VHProd  ('HPair pa pb )) (a,b)  where
    {-# INLINE  basicUnsafeFreeze #-}
    {-# INLINE basicUnsafeThaw #-}
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicUnsafeIndexM #-}


    basicUnsafeFreeze = \(MVHNode  mva mvb) ->
      VHNode <$$$> V.basicUnsafeFreeze mva <***> V.basicUnsafeFreeze mvb


    basicUnsafeThaw = \(VHNode va vb) ->
      MVHNode <$$$> V.basicUnsafeThaw va <***> V.basicUnsafeThaw vb


    basicLength = \(VHNode va _) -> V.basicLength va


    basicUnsafeSlice = \start len (VHNode va vb) ->
      VHNode (V.basicUnsafeSlice start len va) (V.basicUnsafeSlice start len vb)


    basicUnsafeIndexM = \(VHNode va vb) ix ->
      do
          a <- V.basicUnsafeIndexM va ix
          b <- V.basicUnsafeIndexM vb ix
          return (a,b)

instance  (MV.MVector (MVHProd  ('HUnit (V.Mutable v))  ) a ,V.Vector v a)
  => V.Vector (VHProd  ('HUnit v)) a  where

    {-# INLINE  basicUnsafeFreeze #-}
    {-# INLINE basicUnsafeThaw #-}
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicUnsafeIndexM #-}

    basicUnsafeFreeze = \(MVHLeaf mva) ->
      VHLeaf <$$$> V.basicUnsafeFreeze mva
    basicUnsafeThaw = \(VHLeaf va ) ->
      MVHLeaf <$$$> V.basicUnsafeThaw va
    basicLength = \(VHLeaf va ) -> V.basicLength va
    basicUnsafeSlice = \start len (VHLeaf va ) ->
      VHLeaf(V.basicUnsafeSlice start len va)
    basicUnsafeIndexM = \(VHLeaf va) ix ->  V.basicUnsafeIndexM va ix


instance (MV.MVector mv a) => MV.MVector (MVHProd  ('HUnit mv )) a where
  basicLength = \ (MVHLeaf mva) -> MV.basicLength mva
  {-# INLINE basicLength #-}

  basicUnsafeSlice = \ start len (MVHLeaf mva  )->
    MVHLeaf (MV.basicUnsafeSlice start len mva)
  {-# INLINE basicUnsafeSlice #-}

  basicOverlaps = \ (MVHLeaf mva ) (MVHLeaf mva2 )-> (MV.basicOverlaps mva mva2)
  {-# INLINE basicOverlaps #-}

  basicUnsafeNew =
      \ size ->
          MVHLeaf <$$$> MV.basicUnsafeNew size
  {-# INLINE basicUnsafeNew #-}

  basicUnsafeReplicate =
      \ size a ->
         MVHLeaf <$$$>
            MV.basicUnsafeReplicate size a
  {-# INLINE basicUnsafeReplicate #-}

  basicUnsafeRead = \(MVHLeaf mva ) ix ->   MV.basicUnsafeRead mva ix
  {-#INLINE basicUnsafeRead #-}

  basicUnsafeWrite = \ (MVHLeaf mva ) ix a  ->
    do
      MV.basicUnsafeWrite mva ix a
      return ()
  {-#INLINE basicUnsafeWrite #-}

  {-#INLINE basicUnsafeGrow #-}
  basicUnsafeGrow = \ (MVHLeaf mva ) growth ->
      MVHLeaf <$$$> MV.basicUnsafeGrow mva growth



instance (MV.MVector (MVHProd pra) a,MV.MVector (MVHProd  prb) b)
  => MV.MVector (MVHProd  ('HPair pra prb)) (a,b) where

  basicLength = \ (MVHNode mva _) -> MV.basicLength mva
  {-# INLINE basicLength #-}

  basicUnsafeSlice = \ start len (MVHNode mva mvb )->
    MVHNode (MV.basicUnsafeSlice start len mva) (MV.basicUnsafeSlice start len mvb)
  {-# INLINE basicUnsafeSlice #-}

  basicOverlaps = \ (MVHNode mva mvb) (MVHNode mva2 mvb2)-> (MV.basicOverlaps mva mva2) || (MV.basicOverlaps mvb mvb2)
  {-# INLINE basicOverlaps #-}

  basicUnsafeNew =
      \ size ->
          MVHNode <$$$> MV.basicUnsafeNew size <***> MV.basicUnsafeNew size
  {-# INLINE basicUnsafeNew #-}

  basicUnsafeReplicate =
      \ size (a,b) ->
         MVHNode <$$$>
            MV.basicUnsafeReplicate size a <***>
            MV.basicUnsafeReplicate size b
  {-# INLINE basicUnsafeReplicate #-}

  basicUnsafeRead = \(MVHNode mva mvb) ix ->
    (,) <$$$>  MV.basicUnsafeRead mva ix <***> MV.basicUnsafeRead mvb ix

  {-#INLINE basicUnsafeRead #-}

  basicUnsafeWrite = \ (MVHNode mva mvb) ix (a,b) ->
    do
      MV.basicUnsafeWrite mva ix a
      MV.basicUnsafeWrite mvb ix b
      return ()
  {-#INLINE basicUnsafeWrite #-}

  {-#INLINE basicUnsafeGrow #-}
  basicUnsafeGrow = \ (MVHNode mva mvb) growth ->
      MVHNode <$$$> MV.basicUnsafeGrow mva growth <***>
          MV.basicUnsafeGrow mvb growth





