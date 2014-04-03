\begin{code}
module Numerical.LinearAlgebra.Solver.Direct where

    
\end{code}


the most basic of linear system solvers are forward and backward substitution.

Most direct (not iterative) solvers work by decomposing a Matrix \(M\)
into a product of \( U \) upper triangular,    \( L \) lower triangular,
\( D \) diagonal,  \( P \) permutation , and \( Q \) matrices. 
These decompositions are called \emph{matrix factorizations}. 
\footnote{NB: There are other representations for factorizations, but these cover the
standard meat and potatoes of most matrix factorizations}

So lets walk through some basic properties then get to solving them!

\begin{enumerate}
    \item We are considering only full rank linear systems right now, so \emph{square}
    \( n\times n \) sized matrices. 
    
    \item For Diagonal and Triangular Matrices, we can can assume every 
    entry on the diagonal is nonzero. (Otherwise they wont be full rank)
    
    \item Unitary matrices have the special property that their complex transpose 
    (hermitian adjoint) is the inverse matrix! This gives them fantastic numerical properties!
        Hence solving a unitary system of equations is exactly as complex as Matrix Vector Product.
    
    \item Permutation matrices for \( n\times n \) matrices are modeled as length   \( n \)
    vectors that map the integer range \( [n]\rightarrow [n] \), simply reversing the arrow
    gives us the inverse (permutation) matrix \(  [n] \leftarrow [n] \)
    
    \item For a diagonal matrix \( D \), assuming that the absolute values of the diagonal entries
      are bounded away from \(  0 \) and \( \infty \),  we can solve a diagonal system by
      performing matrix product with the 


\end{enumerate}

\section{Explicit Inverses: Just say no}
Never ever form an explicit matrix inverse. Please. Just don't. Ever. If your code 
ever uses a solver to indirectly build a matrix inverse, your answers will 

Part of the subtlety is the question: ``When is an approximate result zero or not?'',
