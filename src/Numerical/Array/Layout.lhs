
\begin{code}

module Numerical.Array.Layout(
  module Numerical.Array.Layout.Base
  ,module Numerical.Array.Layout.Dense
  --,module Numerical.Array.Layout.Sparse
  ) where


import  Numerical.Array.Layout.Base
import  Numerical.Array.Layout.Dense
--import  Numerical.Array.Layout.Sparse
\end{code}



class Layout form rank => LayoutBuilder form rank | form -> rank where
  type Tranposed form

  -- this  needs to be in the builder class because i need to resort
  transposedLayout ::  (form ~ Tranposed transform,transform~Tranposed form)=> form  -> transform

  -- this isn't quite right, would like to require the stream/list of indices only
  -- when dealing with s
  buildLayout :: Shape rank Int -> Maybe [Shape rank Int]->  form
