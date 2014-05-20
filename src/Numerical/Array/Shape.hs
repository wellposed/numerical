{-# LANGUAGE DataKinds, GADTs, TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE ExplicitForAll  #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- {-# LANGUAGE OverlappingInstances #-}

module Numerical.Array.Shape(Shape(..)
    ,foldl
    ,foldr
    ,foldl'
    ,foldl1
    ,foldr1
    ,map
    ,map2
    ,reverseShape
    ,Nat(..)
    ,shapeSize
    ,SNat(..)
    ,weaklyDominates
    ,strictlyDominates
    ,shapeToList
    ,Index
    ,UV.Vector(UV.V_ShapeZ,UV.V_ShapeSZ,UV.V_ShapeSSN)
    ,UV.MVector(UV.MV_ShapeZ,UV.MV_ShapeSZ,UV.MV_ShapeSSN)
    ,T.Traversable(..)
    ,backwards
    )
    where

--import Data.Data
import Data.Typeable
import Data.Data

import qualified Data.Functor as Fun
import qualified  Data.Foldable as F
import qualified Control.Applicative as A
import Control.Monad (liftM)
import qualified Data.Traversable as T

--import Control.NumericalMonad.State.Strict
import Control.NumericalApplicative.Backwards

import Numerical.Nat

import Prelude hiding  (map,foldl,foldr,init,scanl,scanr,scanl1,scanr1,foldl1,foldr1)

import qualified Foreign.Storable  as Store
import qualified Foreign.Ptr as Ptr

import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as GMV

{-
Shape may get renamed to Index in the near future!

PSA: do not take the INLINE pragmas as a style suggestion.
The only reason for the INLINEs, SPECIALIZE and the
nonrecursive type class definitions of operations
in this module are because shape will be used in the inner loops of
array indexing heavy computations,

-}


 {-
the concern basically boils down to "will it specialize / inline well"

 -}


{-
should explore using the Reverse and Backwards transformers in the
Transformers package, but not right now

note also the *Tup operations could be done with a more general State monad
for the tupled accumulation parameter. If theres no perf regression, should
move to using that instead.

-}


infixr 3 :*

type Index rank = Shape rank Int

data Shape (rank :: Nat) a where
    Nil  :: Shape Z a
    (:*) ::  !(a) -> !(Shape r a ) -> Shape  (S r) a
        --deriving  (Show)

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 707
deriving instance Typeable Shape

#elif defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 707

deriving instance Typeable2 Shape

#endif

nilShapeConstrRep :: Constr
nilShapeConstrRep    = mkConstr shapeDataTypeRep "Nil" [] Prefix
consShapeConstrRep :: Constr
consShapeConstrRep   = mkConstr shapeDataTypeRep ":*" [] Infix

shapeDataTypeRep :: DataType
shapeDataTypeRep = mkDataType "Numerical.Array.Shape.Shape" [nilShapeConstrRep,consShapeConstrRep]


--deriving instance (Data a, Typeable n ) => Data (Shape n a)
--  gfoldl f z xs = gfoldl f z (shapeToList xs)

--  --gfoldl _ z Nil = z Nil
--  --gfoldl f z (x :* xs) = z (:*)  `f`  x `f` xs

instance (Data a,Typeable Z) =>  Data (Shape Z a) where
    gfoldl _ z Nil = z Nil
    gunfold _ z _  = z Nil -- not sure if _ z _ is the right one, but typechecks
    dataTypeOf _  = shapeDataTypeRep
    toConstr _ = nilShapeConstrRep

instance (Data a, Data (Shape n a), Typeable (S n))=> Data (Shape (S n) a ) where
    gfoldl k z (a :* b) = (z (:*) `k` a) `k` b
    gunfold k z _ = k (k (z (:*)))
    dataTypeOf _ = shapeDataTypeRep
    toConstr _   = consShapeConstrRep


-- figure this out!
--look at  http://hackage.haskell.org/package/HList-0.3.4.1/docs/src/Data-HList-Data.html
--and https://hackage.haskell.org/package/base-4.3.1.0/docs/Data-Data.html#nilConstr
-- for examples
--instance Data a => Data (Shape Z a) where
  --gfoldl

--deriving instance Data (Shape Z a)
--deriving instance (Data a,Data (Shape n a))=> (Data (Shape (S n) a))

{-
too much work to do data instance with pre 7.8 typeable
-}



instance  Eq (Shape Z a) where
    (==) _ _ = True
    {-#INLINE (==)#-}
instance (Eq a,Eq (Shape s a))=> Eq (Shape (S s) a )  where
    (==)  (a:* as) (b:* bs) =  (a == b) && (as == bs )
    {-#INLINE (==)#-}
instance  Show (Shape Z a) where
    show _ = "Nil"

instance (Show a, Show (Shape s a))=> Show (Shape (S s) a) where
    show (a:* as) = show a  ++ " :* " ++ show as

-- at some point also try data model that
-- has layout be dynamically reified, but for now
-- keep it phantom typed for sanity / forcing static dispatch.
-- NB: may need to make it more general at some future point
--data Strided r a lay = Strided {   getStrides :: Shape r a   }

-- may want to typeclassify this?






shapeToList :: Shape n a -> [a]
shapeToList Nil = []
shapeToList (a:* as) = a : (shapeToList as )


{-
the Traversable instance for shape needs both Z and S Z base
cases to interact nicely with the instances defined for
foldable
-}

instance T.Traversable (Shape Z) where
  traverse = \ _ Nil -> A.pure Nil
  {-# INLINE traverse #-}
  sequenceA = T.traverse id
  mapM f = A.unwrapMonad . T.traverse (A.WrapMonad . f)
  sequence = T.mapM id
  {-#INLINE sequenceA #-}
  {-#INLINE mapM #-}
  {-#INLINE sequence #-}


instance  T.Traversable (Shape (S Z)) where
  traverse = \ f (a:* as) ->  (:*) A.<$> f a A.<*> T.traverse f as
  {-# INLINE traverse #-}
  sequenceA = T.traverse id
  mapM f = A.unwrapMonad . T.traverse (A.WrapMonad . f)
  sequence = T.mapM id
  {-#INLINE sequenceA #-}
  {-#INLINE mapM #-}
  {-#INLINE sequence #-}

instance T.Traversable (Shape (S n)) => T.Traversable (Shape (S (S n))) where
  traverse = \ f (a:* as) ->  (:*) A.<$> f a A.<*> T.traverse f as
  {-#INLINE traverse #-}
  sequenceA = T.traverse id
  mapM f = A.unwrapMonad . T.traverse (A.WrapMonad . f)
  sequence = T.mapM id
  {-#INLINE sequenceA #-}
  {-#INLINE mapM #-}
  {-#INLINE sequence #-}

backwards :: (T.Traversable t, A.Applicative f) =>
      ((a -> Backwards f b) -> t a -> Backwards f (t b))
        -> ((a -> f b) -> t a -> f (t b))
backwards= \ traver f container ->
    forwards $ traver  (\x -> Backwards $ f x) container
{-#INLINE backwards #-}




--instance Fun.Functor (Shape r) where
--    fmap = mapShape
--    {-#INLINE fmap #-}

instance Fun.Functor (Shape Z) where
    fmap  = \ _ Nil -> Nil
    {-# INLINE  fmap #-}

instance  (Fun.Functor (Shape r)) => Fun.Functor (Shape (S r)) where
    fmap  = \ f (a :* rest) -> f a :* ( Fun.fmap f rest )
    {-# INLINE  fmap  #-}
instance  A.Applicative (Shape Z) where
    pure = \ _ -> Nil
    {-# INLINE  pure  #-}
    (<*>) = \ _  _ -> Nil
    {-# INLINE  (<*>) #-}
instance  A.Applicative (Shape r)=> A.Applicative (Shape (S r)) where
    pure = \ a -> a :* (A.pure a)
    {-# INLINE pure #-}
    (<*>) = \ (f:* fs) (a :* as) ->  f a :* ((A.<*>)) fs as
    {-# INLINE  (<*>) #-}

{-
only doing Foldable for ranks >= 1 does mean that
we dont get the cute "rank zero arrays are references"
property. But want foldr1 and foldl1 to always succeed

lets try having rank 0 anyways, i'll be happier if i can support it

-}

instance    F.Foldable (Shape  Z) where
    foldl' = \ _  !init _->  init
    foldr'  = \ _ !init _ ->  init
    foldl  = \ _ init _->  init
    foldr  = \ _ init _->   init
    {-# INLINE foldMap  #-}
    {-#  INLINE foldl #-}
    {-#  INLINE foldr  #-}
    {-# INLINE foldl' #-}
    {-#  INLINE foldr'  #-}
    foldr1 = \ _ _ -> error "you can't call foldr1 on a rank Z(ero) Shape"
    foldl1 =  \_ _  ->  error "you can't call foldl1 on a rank Z(ero) Shape"


instance    F.Foldable (Shape  (S Z)) where
    foldl' = \ f !init (a:*Nil)->  f init a
    foldr'  = \ f !init (a:*Nil)->  f a init
    foldl  = \ f init (a:*Nil)->  f init a
    foldr  = \ f init (a:*Nil)->  f a init
    {-# INLINE foldMap  #-}
    {-#  INLINE foldl #-}
    {-#  INLINE foldr  #-}
    {-# INLINE foldl' #-}
    {-#  INLINE foldr'  #-}
    foldr1 = \ _ (a:* Nil) -> a
    foldl1 =  \ _ (a:* Nil) -> a
    {-#  INLINE foldl1 #-}
    {-#  INLINE foldr1 #-}
instance ( F.Foldable (Shape (S r)) )=> F.Foldable (Shape (S (S r))) where
    foldl' = \ f  init (a:* as) -> F.foldl' f (f init a) as
    foldr' = \f !init (a :* as ) -> f a $!  F.foldr' f init as
    foldl  = \ f  init (a:* as) -> F.foldl' f (f init a) as
    foldr  = \ f  init (a:* as) ->   f a $!  F.foldr f init as
    foldl1 = \ f (a:* as) -> F.foldl' f a as
    {-# INLINE foldMap  #-}
    {-# INLINE foldl #-}
    {-# INLINE foldr  #-}
    {-# INLINE foldl' #-}
    {-# INLINE foldr'  #-}
    {-# INLINE foldl1 #-}
    {-# INLINE foldr1 #-}





{- when you lift a toral order onto vectors, you get
interesting partial order -}

weaklyDominates, strictlyDominates :: (Ord a, A.Applicative  (Shape n), F.Foldable (Shape n) )=>
                        Shape n a -> Shape n a -> Bool
weaklyDominates = \major minor -> foldl (&&) True $! map2 (>=)  major minor
strictlyDominates  = \major minor -> foldl (&&) True $! map2 (>)  major minor
{-# INLINE weaklyDominates #-}
{-# INLINE strictlyDominates #-}

{-# INLINE reverseShape #-}
reverseShape :: Shape n a -> Shape n a
reverseShape Nil = Nil
reverseShape r@(_ :* Nil)= r
reverseShape (a:* b :* Nil) = b:* a :* Nil
reverseShape (a:* b :* c:* Nil )=  c :* b :* a :* Nil
reverseShape (a:* b :* c :* d :* Nil)= d :* c :* b :* a :* Nil
reverseShape list = go SZero Nil list
  where
    go :: SNat n1 -> Shape n1  a-> Shape n2 a -> Shape (n1 + n2) a
    go snat acc Nil = gcastWith (plus_id_r snat) acc
    go snat acc (h :* (t :: Shape n3 a)) =
      gcastWith (plus_succ_r snat (Proxy :: Proxy n3))
              (go (SSucc snat) (h :* acc) t)


{-
TODO: abstract out all the different unrolled cases i have


-}




{-# INLINE map2 #-}
map2 :: forall a b c r . (A.Applicative (Shape r))=>   (a->b ->c) -> (Shape r a) -> (Shape r b) -> (Shape r c )
map2  = \ f shpa shpb -> f A.<$> shpa  A.<*> shpb


{-# INLINE map #-}
map:: forall a b r . (A.Applicative (Shape r))=> (a->b) -> (Shape r a )->( Shape r b)
map  =  \ f shp -> f A.<$> shp



{-# INLINE  foldr #-}
foldr :: forall a b r . (F.Foldable (Shape r))=>  (a->b-> b) -> b -> Shape r a -> b
foldr  = \ f init shp -> F.foldr  f init shp




{-# INLINE  foldl #-}
foldl :: forall a b r. (F.Foldable (Shape r))=> (b-> a -> b) -> b -> Shape r a -> b
foldl  = \ f init shp -> F.foldl f init shp


{-# INLINE foldl' #-}
foldl' :: forall a b r . (F.Foldable (Shape r))=> (b-> a -> b) -> b -> Shape r a -> b
foldl' = \ f init shp -> F.foldl' f init shp

{-# INLINE  foldr1 #-}
foldr1 :: forall b r . (F.Foldable (Shape (S r)))=>  (b->b-> b)  -> Shape (S r) b -> b
foldr1  = \ f  shp -> F.foldr1  f  shp




{-# INLINE  foldl1 #-}
foldl1 :: forall  b r. (F.Foldable (Shape (S r)))=> (b-> b -> b)  -> Shape (S r) b -> b
foldl1  = \ f  shp -> F.foldl1 f  shp





instance Store.Storable a =>Store.Storable (Shape (S Z) a) where
    {-#INLINE sizeOf#-}
    sizeOf = \ _ ->  (Store.sizeOf (undefined :: a))
    -- might want to boost the alignment, but ignore for now
    {-# INLINE alignment #-}
    alignment = \ _ -> Store.alignment (undefined :: a )
    {-# INLINE peek #-}
    peek = \ptr -> do  res <- Store.peek (Ptr.castPtr ptr) ; return (res :* Nil)
    {-# INLINE poke #-}
    poke = \ptr (a:*_) -> Store.poke (Ptr.castPtr ptr) a
    {-# INLINE pokeElemOff #-}
    {-# INLINE peekElemOff #-}
    peekElemOff = \ ptr off -> Store.peekByteOff ptr (off * Store.sizeOf (undefined ::  a ))
    pokeElemOff ptr off val = Store.pokeByteOff ptr (off * Store.sizeOf val) val

    peekByteOff ptr off = Store.peek (ptr `Ptr.plusPtr` off)
    pokeByteOff ptr off = Store.poke (ptr `Ptr.plusPtr` off)
    {-# INLINE peekByteOff #-}
    {-# INLINE pokeByteOff #-}


instance (Store.Storable a,Store.Storable (Shape (S n) a)) =>Store.Storable (Shape (S (S n)) a) where
    {-#INLINE sizeOf#-}
    sizeOf = \ _ ->  Store.sizeOf (undefined :: a)  + Store.sizeOf (undefined :: (Shape (S n) a ))
    -- might want to boost the alignment, but ignore for now
    {-# INLINE alignment #-}
    alignment = \ _ -> Store.alignment (undefined :: a )
    {-# INLINE peek #-}
    peek = \ptr -> do
                a <- Store.peek (Ptr.castPtr ptr) ;
                as <- Store.peek (ptr `Ptr.plusPtr` Store.sizeOf (undefined :: a ))
                return (a:* as)
    {-# INLINE poke #-}
    poke = \ptr (a:*as ) -> do
                        Store.poke (Ptr.castPtr ptr) a
                        Store.poke (ptr `Ptr.plusPtr` Store.sizeOf (undefined :: a )) as
    {-# INLINE pokeElemOff #-}
    {-# INLINE peekElemOff #-}
    peekElemOff = \ ptr off -> Store.peekByteOff ptr (off * Store.sizeOf (undefined :: (Shape (S (S n)) a) ))
    pokeElemOff ptr off val = Store.pokeByteOff ptr (off * Store.sizeOf val) val

    peekByteOff ptr off = Store.peek (ptr `Ptr.plusPtr` off)
    pokeByteOff ptr off = Store.poke (ptr `Ptr.plusPtr` off)
    {-# INLINE peekByteOff #-}
    {-# INLINE pokeByteOff #-}

-- this instance is a bit weird and should never be used
-- but probably legal
instance Store.Storable a =>Store.Storable (Shape Z a) where
    {-#INLINE sizeOf#-}
    sizeOf = \ _ ->  0
    -- might want to boost the alignment, but ignore for now
    {-# INLINE alignment #-}
    alignment = \ _ -> Store.alignment (undefined :: a )
    {-# INLINE peek #-}
    peek = \ _  -> return Nil
    {-# INLINE poke #-}
    poke = \ _  _-> return ()
    {-# INLINE pokeElemOff #-}
    {-# INLINE peekElemOff #-}
    peekElemOff = \ _ _  -> return Nil
    pokeElemOff = \ _ _ _  -> return ()

    peekByteOff  = \ _ _ -> return Nil
    pokeByteOff  = \ _ _ _ -> return ()
    {-# INLINE peekByteOff #-}
    {-# INLINE pokeByteOff #-}

{-# INLINE shapeSize #-}
shapeSize :: F.Foldable (Shape n)=>Shape n a -> Int
shapeSize  = \ as -> ( F.foldl (\ct _ -> ct +1) 0 as )



newtype instance UV.MVector s (Shape Z a)  = MV_ShapeZ  Int
newtype instance UV.Vector    (Shape Z a) = V_ShapeZ  Int

newtype instance UV.MVector s (Shape (S Z) a)  = MV_ShapeSZ (UV.MVector s a)
newtype instance UV.Vector    (Shape (S Z) a) = V_ShapeSZ  (UV.Vector    a)

newtype instance UV.MVector s (Shape (S (S n)) a)  = MV_ShapeSSN (UV.MVector s (a, Shape (S n) a) )
newtype instance UV.Vector    (Shape (S (S n)) a) = V_ShapeSSN  (UV.Vector   (a, Shape (S n) a) )


instance UV.Unbox a => UV.Unbox (Shape Z a)
instance UV.Unbox a =>  UV.Unbox (Shape (S Z) a)
instance (UV.Unbox a,UV.Unbox (Shape (S n) a) )=> UV.Unbox (Shape (S (S n)) a)



instance UV.Unbox a => GMV.MVector UV.MVector  (Shape Z a) where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength  = \ (MV_ShapeZ n) ->  n
  basicUnsafeSlice  = \ _ m (MV_ShapeZ _) -> MV_ShapeZ m
  basicOverlaps = \ _ _  ->  False
  basicUnsafeNew  = \ n -> return (MV_ShapeZ n)
  basicUnsafeRead  = \ (MV_ShapeZ _) _ ->  return Nil
  basicUnsafeWrite  = \ (MV_ShapeZ _) _ Nil ->  return ()
  basicClear = \ _ -> return ()
  basicSet  =  \ (MV_ShapeZ _) Nil -> return ()
  basicUnsafeCopy  = \ (MV_ShapeZ _) (MV_ShapeZ _) ->  return ()
  basicUnsafeGrow  = \ (MV_ShapeZ n) m -> return $ MV_ShapeZ (n+m)

instance UV.Unbox a => GV.Vector UV.Vector  (Shape Z a) where
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeFreeze  = \ (MV_ShapeZ n) ->  return $ V_ShapeZ n
  {-# INLINE basicUnsafeThaw #-}
  basicUnsafeThaw = \ (V_ShapeZ n)  -> return $ MV_ShapeZ n
  {-# INLINE basicLength #-}
  basicLength  = \(V_ShapeZ n) ->  n
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice  = \ _ m (V_ShapeZ _) ->  V_ShapeZ m
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeIndexM = \ (V_ShapeZ _) _  ->  return Nil
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy  = \ (MV_ShapeZ _) (V_ShapeZ _) ->  return ()
  {-# INLINE elemseq #-}
  elemseq  =  \ _ -> seq

instance (UV.Unbox a) => GMV.MVector UV.MVector (Shape (S Z) a) where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength  = \(MV_ShapeSZ v)-> GMV.basicLength v
  basicUnsafeSlice  = \ i n (MV_ShapeSZ v) ->  MV_ShapeSZ $ GMV.basicUnsafeSlice i n v
  basicOverlaps = \ (MV_ShapeSZ v1) (MV_ShapeSZ v2)  ->  GMV.basicOverlaps v1 v2
  basicUnsafeNew  = \ n ->  MV_ShapeSZ `liftM` GMV.basicUnsafeNew n
  basicUnsafeReplicate  = \ n (a:*_) ->  MV_ShapeSZ `liftM` GMV.basicUnsafeReplicate n a
  basicUnsafeRead  = \ (MV_ShapeSZ v) i ->  ( :* Nil ) `liftM` GMV.basicUnsafeRead v i
  basicUnsafeWrite  = \ (MV_ShapeSZ v) i (a:* _) ->  GMV.basicUnsafeWrite v i a
  basicClear = \ (MV_ShapeSZ v)  ->  GMV.basicClear v
  basicSet =  \ (MV_ShapeSZ v) (a:*_)  ->  GMV.basicSet v a
  basicUnsafeCopy  = \ (MV_ShapeSZ v1) (MV_ShapeSZ v2) ->  GMV.basicUnsafeCopy v1 v2
  basicUnsafeMove  = \ (MV_ShapeSZ v1) (MV_ShapeSZ v2) -> GMV.basicUnsafeMove v1 v2
  basicUnsafeGrow  = \ (MV_ShapeSZ v) n ->  MV_ShapeSZ `liftM` GMV.basicUnsafeGrow v n

instance ( UV.Unbox a) => GV.Vector UV.Vector (Shape (S Z) a ) where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze = \ (MV_ShapeSZ v)  ->  V_ShapeSZ `liftM` GV.basicUnsafeFreeze v
  basicUnsafeThaw = \ (V_ShapeSZ v)  ->  MV_ShapeSZ`liftM` GV.basicUnsafeThaw v
  basicLength  = \ (V_ShapeSZ v)-> GV.basicLength v
  basicUnsafeSlice  = \ i n (V_ShapeSZ v) ->  V_ShapeSZ $ GV.basicUnsafeSlice i n v
  basicUnsafeIndexM  = \ (V_ShapeSZ v) i -> ( :* Nil ) `liftM` GV.basicUnsafeIndexM v i
  basicUnsafeCopy   = \ (MV_ShapeSZ mv) (V_ShapeSZ v) -> GV.basicUnsafeCopy mv v
  elemseq  = \ _ (a:*_) z ->    GV.elemseq (undefined :: UV.Vector a) a z


instance (UV.Unbox a,UV.Unbox (Shape (S n) a)) => GMV.MVector UV.MVector (Shape (S (S n)) a) where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength  = \ (MV_ShapeSSN v) -> GMV.basicLength v
  basicUnsafeSlice  = \ i n (MV_ShapeSSN v) -> MV_ShapeSSN $ GMV.basicUnsafeSlice i n v
  basicOverlaps  = \ (MV_ShapeSSN v1) (MV_ShapeSSN v2) -> GMV.basicOverlaps v1 v2
  basicUnsafeNew = \ n -> MV_ShapeSSN `liftM` GMV.basicUnsafeNew n
  basicUnsafeReplicate  = \ n (a :* as) ->  MV_ShapeSSN `liftM` GMV.basicUnsafeReplicate n (a,as)
  basicUnsafeRead = \ (MV_ShapeSSN v) i  ->  uncurry (:*) `liftM` GMV.basicUnsafeRead v i
  basicUnsafeWrite = \(MV_ShapeSSN v) i (a :* as )  -> GMV.basicUnsafeWrite v i (a,as)
  basicClear = \ (MV_ShapeSSN v)  ->  GMV.basicClear v
  basicSet  = \ (MV_ShapeSSN v) (a :* as) ->  GMV.basicSet v (a,as)
  basicUnsafeCopy  = \ (MV_ShapeSSN v1) (MV_ShapeSSN v2) -> GMV.basicUnsafeCopy v1 v2
  basicUnsafeMove  = \ (MV_ShapeSSN v1) (MV_ShapeSSN v2) ->  GMV.basicUnsafeMove v1 v2
  basicUnsafeGrow = \ (MV_ShapeSSN v) n  -> MV_ShapeSSN `liftM` GMV.basicUnsafeGrow v n


instance (UV.Unbox a,UV.Unbox (Shape (S n) a)) =>  GV.Vector UV.Vector (Shape (S (S n)) a) where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze  = \ (MV_ShapeSSN v) ->  V_ShapeSSN `liftM` GV.basicUnsafeFreeze v
  basicUnsafeThaw = \ (V_ShapeSSN v)  ->  MV_ShapeSSN `liftM` GV.basicUnsafeThaw v
  basicLength = \ (V_ShapeSSN v)  -> GV.basicLength v
  basicUnsafeSlice = \ i n (V_ShapeSSN v)  -> V_ShapeSSN $ GV.basicUnsafeSlice i n v
  basicUnsafeIndexM  = \ (V_ShapeSSN v)  i -> uncurry (:*) `liftM` GV.basicUnsafeIndexM v i
  basicUnsafeCopy    =  \ (MV_ShapeSSN mv) (V_ShapeSSN v) -> GV.basicUnsafeCopy mv v
  elemseq = \  _ (a :* as) z ->  GV.elemseq (undefined :: UV.Vector a) a
                       $ GV.elemseq (undefined :: UV.Vector (Shape (S n) a)) as z
