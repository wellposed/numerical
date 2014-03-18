{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}

module Control.Monad.State.ReallyStrict(State,get,put) where 

import qualified Control.Applicative as A 


newtype State s a = MkState (s -> (# a , s #))

unwrap (MkState m)=m 

{-#INLINE get #-}
get :: State s s 
get = MkState (\ !s -> (# s,s #))

{-# INLINE put #-}
put :: s -> State s () 
put = \ !st -> MkState (\_ -> (# (), st #))

{-#INLINE modify #-}
modify :: (s->s)-> State s ()
modify = \ !f ->  (MkState (\ !s -> (# (), f s #)))

instance Functor (State s) where
    {-#INLINE fmap #-}
    fmap = \ !f !(MkState mf) -> MkState (\ !s -> case mf s of (# v, newS   #)-> (# f v,newS   #) )    

instance A.Applicative (State s) where
    {-# INLINE pure  #-}
    pure = \ !v -> (MkState (\ !s -> (# v , s #)))
    {-# INLINE (<*>) #-}
    (<*>)= \ !(MkState fm) !(MkState vm) -> 
                MkState ( \ !s -> 
                    case  fm s of 
                        (# fv, ns #)-> 
                            case vm s of 
                                (# vv, nns #) -> (# fv vv, nns #) )
instance Monad (State s) where
    {-# INLINE return #-}
    return = A.pure 
    {-# INLINE (>>=) #-}
    (>>=) = \ !(MkState m)  !f -> 
                MkState $! \ !s -> case  m s of 
                    (# v, ns #) -> 
                            case  f v of 
                                (MkState fm)-> fm ns 

