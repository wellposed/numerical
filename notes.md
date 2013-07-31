#notes / initial modelling assumptions

For now, will deal with the crazy that is the normal haskell numerical type classes

At a later point, will likely roll a mini alternative numerical prelude.
For now this restriction means I essentially only have Rational Fields (rational numbers) and 
approximately Real/Complex fields  (floating point numbers).

the main distinction is that only floating point style numbers (or computable reals)
have a notion of square root and the transcendental functions.

Likewise, for now certain operations are only defined for rank 1 and rank 2 arrays
(vectors and matrices), though higher rank analogues may 


would like to at some point change things so matrix mult and dot product
can be defined by by any 

## For now



## shapes layouts and indices
Lets equip the shaps with their layouts, or maybe indexes with the shape+ layout that generated them!
(latter)


## deep embedding backends
wont be supported till after "version zero", though 