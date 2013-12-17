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
wont be supported till after "version zero", though will soon afterwards, but 
some of the scaffolding will be visible in version zero

## why 


# design things 

lets have the fast path for every algorithm be the "no aliasing" case, and for
the imperative versions, bad aliasing (ie the write target and read targe alias 
in an "incoherent / bad" way )


# Array layouts

Most numerical computation focuses on Vectors and Matrices,
which respectively are rank 1 and rank 2 arrays. That said, 

row major 2dim
[   1 2 
    3 4
]


col major 2dim
[
    1 3
    2 4
]



example of nested layout

col major of 2x2 rowmajor (2dim)

[ {1  2  {9  10
   3  4}, 11 12},
   {5  6  {13 14
   7  8},  15 16}
]
--- this is a terrible way to visualize it!


Array ... rank storage  layout  ... elem

layout =  Lay (ColMajor :#  [n3:@ Colmajor, n2:@ RowMajor] )