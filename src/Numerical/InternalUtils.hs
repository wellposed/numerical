{-# LANGUAGE NoImplicitPrelude#-}
module Numerical.InternalUtils(error,currentCallStack,whoCreated) where
import GHC.Stack (errorWithStackTrace,currentCallStack,whoCreated)
import Prelude hiding (error)
error :: String -> a
error = errorWithStackTrace


{-
note well: the stack traces only exist
when doing a profiling build in GHC < 7.9/7.10
-}