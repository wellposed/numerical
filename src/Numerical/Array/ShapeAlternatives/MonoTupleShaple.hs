{-# LANGUAGE PolyKinds   #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Numerical.Types.Shape where


import Numerical.Types.Nat
import Data.Data
import Unsafe.Coerce

import Prelude  hiding (map,foldl,foldr)




{-
not doing the  HLIST style shape because I don't want to have
any pattern matchings going on.

Also would play hell with locality quality in the address translation hackery,
because there'd be an extra load to get those ints!
-}
infixr 3 :*

 {-
the concern basically boils down to "will it specialize / inline well"

 -}
data ShapeAlt (rank :: Nat) where
    Nil  :: ShapeAlt Z
    (:*) :: {-# UNPACK #-} !(Int:: *) -> !(ShapeAlt r) -> ShapeAlt (S r)

deriving instance Show (ShapeAlt rank)

deriving instance Eq (ShapeAlt rank)


#if defined( __GLASGOW_HASKELL__ ) &&  ( __GLASGOW_HASKELL__  >= 707)
deriving instance Typeable (Shape rank)

#endif








type family (TupleRank tup ) :: Nat
type instance TupleRank () = Z
type instance  TupleRank Int = N1
type instance TupleRank (Int,Int) = N2
type instance TupleRank (Int,Int,Int) = N3
type instance TupleRank (Int,Int,Int,Int) =  N4
type instance TupleRank (Int,Int,Int,Int,Int) = N5
type instance TupleRank (Int,Int,Int,Int,Int,Int) = N6
-- easy to add more instances as needed


class Shapable (n :: Nat)  where
    data Shape n
    type  Tuple n
    --type Tuple n = tup
-- this should map an int tuple to their rank n

    toShape :: (tup~Tuple n, n ~ TupleRank tup ) => tup -> Shape n
    fromShape :: (tup~Tuple n, n ~ TupleRank tup ) => Shape n -> tup

    foldl ::  (a -> Int -> a) -> a -> Shape n -> a

    foldr :: (Int -> a -> a ) -> a -> Shape n -> a
{-
add
    scanr :: (Int -> a -> (Int,a))->a -> Shape n -> (Shape n,a)
    scanl :: ()

-}
    -- not sure if I need the  strict variants of
    map :: (Int -> Int) -> Shape n -> Shape n
    zip :: (Int -> Int -> Int ) -> Shape n -> Shape n -> Shape n
    zip2 :: (Int -> Int -> (Int ,Int ))-> Shape n -> Shape n -> (Shape n, Shape n )

-- zero rank is boring but lets include it for completeness
instance Shapeable Z  () where
    data Shape  Z = Shape0
    type Tuple  Z = ()

    toShape = \ _ -> Shape0
    fromShape = \ _ -> ()
    map = \ f Shape0 -> Shape0
    foldl = \ f init  (Shape0) -> init
    foldr = \ f  init (Shape0)  -> init
    zip = \ f _ _ -> Shape0
    zip2 = \f _ _ -> (Shape0,Shape0)

{-| For now I'm not making any thing strict in the shapeable instances,
except for the data Shape n =  unpacked strict Int constructors

-}
instance  Shapeable  N1 Int  where
    data Shape N1  = Shape1  {-# UNPACK #-}  !Int
    type Tuple N1  = Int

    toShape = Shape1
    fromShape = \ (Shape1  x) -> x
    map = \f (Shape1 v1)-> Shape1 $ f v1
    foldl = \ f init  (Shape1 v1) -> f init v1
    foldr = \f init  (Shape1 v1) -> f  v1 init
    zip = \ f (Shape1 a1) (Shape1 b1) -> Shape1 $! f a1 b1
    zip2 =  \ f (Shape1 a1) (Shape1 b1) ->
                let  (ra1,rb1)= f a1 b1  in (Shape1 ra1,Shape1 rb1)


instance Shapeable N2  (Int,Int) where
    data Shape N2 = Shape2 {-# UNPACK #-}  !Int {-# UNPACK #-}  !Int
    type Tuple  N2 = (Int,Int)
    toShape = \ (a,b) -> Shape2 a b
    fromShape = \ (Shape2 a b) -> (a,b)
    map  = \f  (Shape2 v1 v2) -> Shape2 (f v1) (f v2)
    foldl =  \f init (Shape2 v1 v2) ->  init `f` v1 `f` v2
    foldr = \f init  (Shape2 v1 v2) -> f v1 $ f v2 init
    zip = \f (Shape2 a1 a2) (Shape2 b1 b2) -> Shape2 (f a1 b1) (f a2 b2)
    zip2 =
        \f (Shape2 a1 a2) (Shape2 b1 b2) ->
            let
                (ra1,rb1) = f a1 b1
                (ra2,rb2) = f a2 b2
                in (Shape2 ra1 ra2, Shape2 rb1 rb2)

instance Shapeable N3 (Int,Int,Int) where
    data Shape N3 = Shape3 {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-}  !Int
    type Tuple N3 = (Int,Int,Int)
    toShape  =  \ (v1,v2,v3)-> Shape3 v1 v2 v3
    fromShape = \ (Shape3 a b c )-> (a,b,c)
    map  = \f (Shape3 v1 v2 v3) -> Shape3 (f v1) (f v2) (f v3)
    foldl = \ f init (Shape3 v1 v2 v3) -> init `f` v1 `f` v2 `f` v3
    foldr = \ f init (Shape3 v1 v2 v3) -> f v1 $! f v2 $! f v3 init
    zip =  \ f  (Shape3 a1 a2 a3) (Shape3 b1 b2 b3) ->
                 Shape3 (f a1 b1) (f a2 b2) (f a3 b3)
    zip2  = \f (Shape3 a1 a2 a3  ) (Shape3 b1 b2 b3  ) ->
                (let
                    (ra1,rb1)= f a1 b1
                    (ra2,rb2)= f a2 b2
                    (ra3,rb3)= f a3 b3
                    in
                    (Shape3 ra1 ra2 ra3  ,
                            Shape3 rb1 rb2 rb3  ))

instance Shapeable  N4  (Int,Int,Int,Int) where
    data Shape N4 = Shape4 {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-}  !Int
                        {-# UNPACK #-} !Int
    type Tuple N4 = (Int,Int,Int,Int)
    toShape  = \ (v1,v2,v3,v4) -> Shape4 v1 v2 v3 v4
    fromShape = \(Shape4 v1 v2 v3 v4)->(v1,v2,v3,v4)
    map  = \ f (Shape4 v1 v2 v3 v4) -> Shape4 (f v1) (f v2) (f v3) (f v4)
    foldl = \ f init (Shape4 v1 v2 v3 v4  ) ->
                     init `f` v1 `f`  v2 `f` v3 `f` v4
    foldr = \ f init (Shape4 v1 v2 v3 v4) ->
                      f v1  $! f  v2 $! f v3  $! f v4    init
    zip = \f (Shape4 a1 a2 a3 a4 ) (Shape4 b1 b2 b3 b4 ) ->
                    Shape4 (f a1 b1) (f a2 b2) (f a3 b3)  (f a4 b4)
    zip2 =  \f (Shape4 a1 a2 a3 a4  ) (Shape4 b1 b2 b3 b4 ) ->
                (let
                    (ra1,rb1)= f a1 b1
                    (ra2,rb2)= f a2 b2
                    (ra3,rb3)= f a3 b3
                    (ra4,rb4)= f a4 b4
                    in
                    (Shape4 ra1 ra2 ra3 ra4 ,
                            Shape4 rb1 rb2 rb3 rb4 ))


instance Shapeable  N5 (Int,Int,Int,Int,Int) where
    data Shape N5 = Shape5 {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-}  !Int
                        {-# UNPACK #-} !Int {-# UNPACK #-} !Int
    type Tuple N5 = (Int,Int,Int,Int,Int)
    toShape  = \(v1,v2,v3,v4,v5) -> Shape5 v1 v2 v3 v4 v5
    fromShape = \  (Shape5 v1 v2 v3 v4 v5 ) -> (v1,v2,v3,v4,v5)
    map  = \ f (Shape5 v1 v2 v3 v4 v5) -> Shape5 (f v1) (f v2) (f v3) (f v4) (f v5)
    foldl = \ f init (Shape5 v1 v2 v3 v4 v5 ) ->
                     init `f` v1 `f`  v2 `f` v3 `f` v4 `f` v5
    foldr = \ f init (Shape5 v1 v2 v3 v4 v5 ) ->
                      f v1  $! f  v2 $! f v3  $! f v4  $! f v5   init
    zip = \f (Shape5 a1 a2 a3 a4 a5) (Shape5 b1 b2 b3 b4 b5) ->
                    Shape5 (f a1 b1) (f a2 b2) (f a3 b3)  (f a4 b4) (f a5 b5)
    zip2 = \f (Shape5 a1 a2 a3 a4 a5 ) (Shape5 b1 b2 b3 b4 b5) ->
                (let
                    (ra1,rb1)= f a1 b1
                    (ra2,rb2)= f a2 b2
                    (ra3,rb3)= f a3 b3
                    (ra4,rb4)= f a4 b4
                    (ra5,rb5)= f a5 b5
                    in
                        (Shape5 ra1 ra2 ra3 ra4 ra5,
                            Shape5 rb1 rb2 rb3 rb4 rb5 ))


instance Shapeable  N6 (Int,Int,Int,Int,Int,Int) where
    data Shape N6 = Shape6 {-# UNPACK #-}   !Int {-# UNPACK #-}     !Int {-# UNPACK #-}
          !Int {-# UNPACK #-}      !Int {-# UNPACK #-}     !Int {-# UNPACK #-}  !Int


    type Tuple N6 = (Int,Int,Int,Int,Int,Int)

    toShape  = \(v1,v2,v3,v4,v5,v6) -> Shape6 v1 v2 v3 v4 v5 v6
    fromShape = \ (Shape6 v1 v2 v3 v4 v5 v6) -> (v1,v2,v3,v4,v5,v6)
    map  = \ f (Shape6 v1 v2 v3 v4 v5 v6) -> Shape6 (f v1) (f v2) (f v3) (f v4) (f v5) (f v6)
    foldl = \ f init (Shape6 v1 v2 v3 v4 v5 v6) ->
                     init `f` v1 `f`  v2 `f` v3 `f` v4 `f` v5  `f` v6
    foldr = \ f init (Shape6 v1 v2 v3 v4 v5 v6) ->
                      f v1  $! f  v2 $! f v3  $! f v4  $! f v5  $! f v6 init
    zip = \f (Shape6 a1 a2 a3 a4 a5 a6) (Shape6 b1 b2 b3 b4 b5 b6) ->
                    Shape6 (f a1 b1) (f a2 b2) (f a3 b3)  (f a4 b4) (f a5 b5) (f a6 b6)
    zip2 = \f (Shape6 a1 a2 a3 a4 a5 a6) (Shape6 b1 b2 b3 b4 b5 b6) ->
                (let
                    (ra1,rb1)= f a1 b1
                    (ra2,rb2)= f a2 b2
                    (ra3,rb3)= f a3 b3
                    (ra4,rb4)= f a4 b4
                    (ra5,rb5)= f a5 b5
                    (ra6,rb6)= f a6 b6
                    in
                    (Shape6 ra1 ra2 ra3 ra4 ra5 ra6,
                            Shape6 rb1 rb2 rb3 rb4 rb5 rb6 ))



{-
NB: Should maybe add up to dimension 8 or 10 (or at some joke, 26 for string theory),
but 6 should
-}


{-|
index ideas inspired by repa3 / repa4, but

NOTE: for the the low rank cases (eg 1-2 dim arrays)
should figure out a lighter weight indexing story if that
winds up being widely used and blocking good fusion. Not doing this For now.

However! Writing high level algorithms in a explicitly pointwise indexing
heavy way will be a bad bad idea.

-}







{-
NB, see http://ghc.haskell.org/trac/ghc/ticket/8423
for a provable version, though only works on 7.8
and uses singletons. Should revist at some point

also see
https://gist.github.com/cartazio/6913380 for Ranjit Jhala's formulation
and https://gist.github.com/cartazio/6907168 for Richard Eisenberg's formulation

-}







---- | 'Shaped' lay sh is a Shape meant for representing the Array rank and dimensions sh,
---- with layout lay.
--newtype Shaped lay sh = Shaped sh

---- |  Writing down the common ranks.
--type DIM1 = Shape  One
--type DIM2 = Shape  Two
--type DIM3 = Shape Three



--nilShape :: Shape $(nat 0)-> Int
--nilShape _ = 0






