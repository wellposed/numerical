


{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Numerical.Array.Layout.Sparse .Builder (
  ) where

--import  Numerical.Array.Layout.Sparse as Sparse
import Numerical.Array.Shape
--import Data.Proxy
import Data.Vector.Generic as VG
import Numerical.Array.Storage



{-
i'm slightly concerned about how the indirection between
storageRep and the underlying vector types will work out,
but lets roll with the indirection for now and drop

-}

class SparseLayout  (Format layout Contiguous rank storageRep) rank
  =>  SparseBuilder layout storageRep (rank :: Nat )  where


    basicSparseValueBuilder ::(VG.Vector v  elem,storageRep~VectorName v,VG.Vector v  Int, v~(StorageVector storageRep) )
        => proxy layout -> proxy storageRep ->Shape rank Int -> [(Shape rank Int, elem)]-> (Format layout Contiguous rank storageRep, v elem)


    --basicSparseBuilder ::proxy layout -> proxy storageRep -> Shape rank Int -> [Shape rank Int]->  Format layout Contiguous rank storagerep




