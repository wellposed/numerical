


module Numerical.World where 



{-| 
Every numerical algorithm runs somewhere.

This could be on a CPU, a GPU,  

-}
-- Native is Just Haskell and Cbits, no external Deps
data Native

-- ForeignNative can have foreign lib deps, 
data ForeignNative