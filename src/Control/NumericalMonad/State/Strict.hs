
module Control.NumericalMonad.State.Strict where

--import Data.Functor.Identity
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Applicative
import Control.Monad
import Control.Monad.Fix


{-

This module is a private copy of the Strict State Monad by Ross Patterson,
patched to unconditionally inline.

Its only purpose is to ensure that certain generic routines in
Numerical.Array.Shape will compositionally unconditionally inline in their use sites

ONLY use if writing generic code in your inner loops

-}


import Data.Foldable (Foldable(foldMap))
import Data.Traversable (Traversable(traverse))

-- | Identity functor and monad.
newtype Identity a = Identity { runIdentity :: a }

-- ---------------------------------------------------------------------------
-- Identity instances for Functor and Monad

instance Functor Identity where
    fmap f m = Identity (f (runIdentity m))
    {-# INLINE fmap #-}

instance Foldable Identity where
    foldMap f (Identity x) = f x
    {-# INLINE foldMap #-}

instance Traversable Identity where
    traverse f (Identity x) = Identity <$> f x
    {-# INLINE traverse #-}

instance Applicative Identity where
    pure a = Identity a
    {-# INLINE pure #-}
    Identity f <*> Identity x = Identity (f x)
    {-# INLINE (<*>) #-}

instance Monad Identity where
    return a = Identity a
    {-# INLINE return #-}
    m >>= k  = k (runIdentity m)
    {-# INLINE (>>=)#-}
instance MonadFix Identity where
    mfix f = Identity (fix (runIdentity . f))
    {-# INLINE mfix  #-}


-- ---------------------------------------------------------------------------
-- | A state monad parameterized by the type @s@ of the state to carry.
--
-- The 'return' function leaves the state unchanged, while @>>=@ uses
-- the final state of the first computation as the initial state of
-- the second.
type State s = StateT s Identity

-- | Construct a state monad computation from a function.
-- (The inverse of 'runState'.)
state :: Monad m
      => (s -> (a, s))  -- ^pure state transformer
      -> StateT s m a   -- ^equivalent state-passing computation
state  = \f -> StateT (return . f)
{-# INLINE state #-}

-- | Unwrap a state monad computation as a function.
-- (The inverse of 'state'.)
runState :: State s a   -- ^state-passing computation to execute
         -> s           -- ^initial state
         -> (a, s)      -- ^return value and final state
runState  = \ m ->  runIdentity . runStateT m
{-# INLINE runState#-}


-- | Evaluate a state computation with the given initial state
-- and return the final value, discarding the final state.
--
-- * @'evalState' m s = 'fst' ('runState' m s)@
evalState :: State s a  -- ^state-passing computation to execute
          -> s          -- ^initial value
          -> a          -- ^return value of the state computation
evalState  = \m s -> fst (runState m s)
{-# INLINE evalState #-}

-- | Evaluate a state computation with the given initial state
-- and return the final state, discarding the final value.
--
-- * @'execState' m s = 'snd' ('runState' m s)@
execState :: State s a  -- ^state-passing computation to execute
          -> s          -- ^initial value
          -> s          -- ^final state
execState  =  \m s -> snd (runState m s)
{-# INLINE execState#-}

-- | Map both the return value and final state of a computation using
-- the given function.
--
-- * @'runState' ('mapState' f m) = f . 'runState' m@
mapState :: ((a, s) -> (b, s)) -> State s a -> State s b
mapState   = \ f ->  mapStateT (Identity . f . runIdentity)
{-# INLINE mapState #-}

-- | @'withState' f m@ executes action @m@ on a state modified by
-- applying @f@.
--
-- * @'withState' f m = 'modify' f >> m@
withState :: (s -> s) -> State s a -> State s a
withState = \f st -> withStateT f st
{-# INLINE withState #-}
-- ---------------------------------------------------------------------------
-- | A state transformer monad parameterized by:
--
--   * @s@ - The state.
--
--   * @m@ - The inner monad.
--
-- The 'return' function leaves the state unchanged, while @>>=@ uses
-- the final state of the first computation as the initial state of
-- the second.
newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

-- | Evaluate a state computation with the given initial state
-- and return the final value, discarding the final state.
--
-- * @'evalStateT' m s = 'liftM' 'fst' ('runStateT' m s)@
evalStateT :: (Monad m) => StateT s m a -> s -> m a
evalStateT  = \ m s -> do
    (a, _) <- runStateT m s
    return a
{-# INLINE evalStateT #-}

-- | Evaluate a state computation with the given initial state
-- and return the final state, discarding the final value.
--
-- * @'execStateT' m s = 'liftM' 'snd' ('runStateT' m s)@
execStateT :: (Monad m) => StateT s m a -> s -> m s
execStateT  = \ m s -> do
    (_, s') <- runStateT m s
    return s'
{-# INLINE  execStateT #-}


-- | Map both the return value and final state of a computation using
-- the given function.
--
-- * @'runStateT' ('mapStateT' f m) = f . 'runStateT' m@
mapStateT :: (m (a, s) -> n (b, s)) -> StateT s m a -> StateT s n b
mapStateT  = \ f m ->  StateT $ f . runStateT m

-- | @'withStateT' f m@ executes action @m@ on a state modified by
-- applying @f@.
--
-- * @'withStateT' f m = 'modify' f >> m@
withStateT :: (s -> s) -> StateT s m a -> StateT s m a
withStateT  = \ f m -> StateT $ runStateT m . f

instance (Functor m) => Functor (StateT s m) where
    fmap = \ f m  ->  StateT $ \ s ->
        fmap (\ (a, s') -> (f a, s')) $ runStateT m s
    {-# INLINE fmap  #-}

instance (Functor m, Monad m) => Applicative (StateT s m) where
    pure = \ a ->return a
    (<*>) = \ a b ->  ap a b

instance (Functor m, MonadPlus m) => Alternative (StateT s m) where
    empty = mzero
    {-# INLINE empty  #-}
    (<|>) = \ a b -> mplus a b
    {-#INLINE (<|>)#-}

instance (Monad m) => Monad (StateT s m) where
    {-# INLINE return #-}
    return  = \ a ->  state $ \s -> (a, s)
    {-# INLINE (>>=)#-}
    (>>=)   = \m k ->  StateT $ \s -> do
        (a, s') <- runStateT m s
        runStateT (k a) s'
    fail str = StateT $ \_ -> fail str

instance (MonadPlus m) => MonadPlus (StateT s m) where
    mzero       = StateT $ \_ -> mzero
    {-# INLINE mzero #-}
    mplus = \ m n -> StateT $ \s -> runStateT m s `mplus` runStateT n s
    {-# INLINE mplus #-}
instance (MonadFix m) => MonadFix (StateT s m) where
    mfix  = \ f -> StateT $ \s -> mfix $ \ ~(a, _) -> runStateT (f a) s
    {-# INLINE mfix #-}

instance MonadTrans (StateT s) where
    {-#INLINE lift #-}
    lift   = \ m ->  StateT $ \s -> do
        a <- m
        return (a, s)

instance (MonadIO m) => MonadIO (StateT s m) where
    liftIO = lift . liftIO

-- | Fetch the current value of the state within the monad.
get :: (Monad m) => StateT s m s
get = state $ \s -> (s, s)
{-# INLINE get #-}

-- | @'put' s@ sets the state within the monad to @s@.
put :: (Monad m) => s -> StateT s m ()
put  = \s ->  state $ \_ -> ((), s)
{-# INLINE put #-}

-- | @'modify' f@ is an action that updates the state to the result of
-- applying @f@ to the current state.
--
-- * @'modify' f = 'get' >>= ('put' . f)@
modify :: (Monad m) => (s -> s) -> StateT s m ()
modify = \f ->  state $ \s -> ((), f s)
{-# INLINE modify #-}

-- | Get a specific component of the state, using a projection function
-- supplied.
--
-- * @'gets' f = 'liftM' f 'get'@
gets :: (Monad m) => (s -> a) -> StateT s m a
gets = \ f ->  state $ \s -> (f s, s)
{-# INLINE gets #-}

