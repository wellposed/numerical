
\begin{code}
-- {-# LANGUAGE PolyKinds   #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables#-}
-- {-#  LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-#  LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-#  LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}


module Numerical.Array.Generic.Internal where

import Control.Monad.Primitive ( PrimMonad, PrimState )
import qualified Numerical.Array.Layout as L 
import Numerical.Array.Layout (Address(..),Locality(..),Direct(..))
import Numerical.Array.Shape 
import GHC.Prim(Constraint)

import qualified Data.Vector.Storable.Mutable as SM
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.Vector.Mutable as BM
\end{code}


NOt sure if I'll ever ever actually use Internal here,
i'm kinda in favor of just doing a darn boot file