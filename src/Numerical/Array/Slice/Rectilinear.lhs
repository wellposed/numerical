
\begin{code}
module Numerical.Array.Slice.Rectilinear where    



data Range {low:: {-#UNPACK #-}!Int,high:: {-#UNPACK#-}!Int }
{-

YUCKKKK, need to fix this.... this will blow anyones complexity budget
perhaps need a more implicit way of handling 
world & rep & arr



Array arr world rep lay local rank el --- seems to complex..
lets have the Data Family be indexed by these, but the type class 
should just have the 
-}
class Array arr world rep lay local rank el=> RectlinearSlice  a where 
    majorAxSlice :: Array world rep lay local rank el  ->Range -> 

class MutArray marr => MutRectlinearSlice  a where 
    
\end{code}