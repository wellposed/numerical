{-# LANGUAGE PolyKinds   #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Numerical.Types.Operations where

import Control.Monad.Primitive ( PrimMonad, PrimState )
import Numerical.Types.Array

