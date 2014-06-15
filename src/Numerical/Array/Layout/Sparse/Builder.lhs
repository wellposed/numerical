

\begin{code}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Numerical.Array.Layout.Sparse .Builder (
  ) where

--import  Numerical.Array.Layout.Sparse as Sparse
import Numerical.Array.Shape
import Data.Proxy
import Data.Vector.Generic as VG
import Numerical.Array.Storage

--class SparseBuilder form rank | form -> rank where

{-


-}

class SparseLayout  (Format layout Contiguous rank storagerep) rank
  =>  SparseBuilder elem layout storageRep (rank :: Nat )  where


    basicSparseValueBuilder ::(VG.Vector v  elem, v~(StorageVector storageRep) )
        => proxy layout -> proxy storageRep ->Shape rank Int -> [(Shape rank Int, elem)]-> (Format layout Contiguous rank storageRep, StorageVector storageRep elem)
    --basicSparseBuilder ::proxy layout -> proxy storageRep -> Shape rank Int -> [Shape rank Int]->  Format layout Contiguous rank storagerep




\end{code}
