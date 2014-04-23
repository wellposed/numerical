\begin{code}


module Numerical.Array.Generic.Phased  where 

import Numerical.Array.Generic.Mutable as MA
import Numerical.Array.Generic.Pure as  A 



\end{code}



class (MA.MutableArray marr rank a, A.Array arr rank a)=> PhasedArray marr arr rank a  
                                |  arr -> marr, marr -> arr, marr arr -> rank  where 



this is just one idea