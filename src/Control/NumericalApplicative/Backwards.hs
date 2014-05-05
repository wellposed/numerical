

module Control.NumericalApplicative.Backwards where

import Prelude hiding (foldr, foldr1, foldl, foldl1)
import qualified Control.Applicative as A
import Data.Foldable as F
import Data.Traversable as T

-- | The same functor, but with an 'Applicative' instance that performs
-- actions in the reverse order.
newtype Backwards f a = Backwards { forwards :: f a }

-- | Derived instance.
instance (Functor f) => Functor (Backwards f) where
    fmap f (Backwards a) = Backwards (fmap f a)
    {-# INLINE fmap #-}

-- | Apply @f@-actions in the reverse order.
instance (A.Applicative f) => A.Applicative (Backwards f) where
    pure a = Backwards (A.pure a)
    {-# INLINE pure #-}
    Backwards f <*> Backwards a = Backwards (a <**> f)
    {-# INLINE (<*>) #-}


-- | Try alternatives in the same order as @f@.
instance (A.Alternative f) => A.Alternative (Backwards f) where
    empty = Backwards A.empty
    Backwards x <|> Backwards y = Backwards (x A.<|> y)

-- | Derived instance.
instance (Foldable f) => Foldable (Backwards f) where
    foldMap f (Backwards t) = foldMap f t
    foldr f z (Backwards t) = foldr f z t
    foldl f z (Backwards t) = foldl f z t
    foldr1 f (Backwards t) = foldl1 f t
    foldl1 f (Backwards t) = foldr1 f t

-- | Derived instance.
instance (Traversable f) => Traversable (Backwards f) where
    traverse f (Backwards t) = fmap Backwards (traverse f t)
    sequenceA (Backwards t) = fmap Backwards (sequenceA t)
    mapM f = A.unwrapMonad . T.traverse (A.WrapMonad . f)
    sequence = T.mapM id
    {-#INLINE traverse #-}
    {-#INLINE sequenceA #-}
    {-#INLINE mapM #-}
    {-#INLINE sequence #-}

(<**>) :: A.Applicative f => f a -> f (a -> b) -> f b
(<**>) = liftA2 (flip ($))
{-# INLINE (<**>) #-}

liftA2 :: A.Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = f `fmap` a A.<*> b
{-# INLINE liftA2 #-}

