\begin{code}

{-# LANGUAGE TypeFamilies #-}
module Numerical.Array.Storage(Boxed,Unboxed,Stored,StorageVector) where

import qualified Data.Vector as BV
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Unboxed as UV


data Boxed
data Unboxed
data Stored


type family StorageVector sort  :: (  * -> * )

type instance StorageVector Boxed = BV.Vector

type instance StorageVector Unboxed = UV.Vector

type instance StorageVector Stored = SV.Vector


\end{code}
