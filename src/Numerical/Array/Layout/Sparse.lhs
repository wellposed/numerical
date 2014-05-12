
the following (currently 5) sparse formats will live here


DirectSparse 1dim



one subtlety and a seemingly subtle point will be
that contiguous / inner contiguous sparse arrays
in  2dim  (and  1dim) will have an ``inner dimension" shift int.
This is so that slices can  be zero copy on *BOTH* the array of values,
and the Format indexing array machinery.

Note that in the 2dim case, it still wont quite be zero copy, because the
offsets into the inner dimension lookup table (not quite the right word)
will have to change when a general slice is used rather than a slice
that acts only on the outermost dimension.

\begin{code}
module Numerical.Array.Layout.Sparse(SparseLayout(..)) where



class Layout form rank  => SparseLayout form  (rank :: Nat) | form -> rank  where




    basicToSparseAddress :: form  -> Shape rank Int -> Maybe  Address


    basicToSparseIndex :: form -> Address -> Shape rank Int


    basicNextAddress :: form  -> Address ->  Address

    basicNextIndex :: form  -> Shape rank Int ->Shape rank Int



\end{code}



CSR and CSC go here, and their version of lookups and next address and next index
