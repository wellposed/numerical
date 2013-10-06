{-# LANGUAGE PolyKinds   #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Numerics.Types.Operations where 

import Control.Monad.Primitive ( PrimMonad, PrimState )
import Numerics.Types.Array

