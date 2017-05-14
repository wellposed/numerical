{-# LANGUAGE NoImplicitPrelude#-}
module Numerical.InternalUtils(
    error
) where

--import GHC.Stack (errorWithStackTrace,currentCallStack,whoCreated)
import Prelude (error)



{-
note well: the stack traces only exist
when doing a profiling build in GHC < 7.9/7.10
-}
