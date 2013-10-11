

module Numerical.Types.Phased where

{-
An array storage type + world pair is said to have Phased instance
when there are both Array and MArray instances for that storage +world pair.

it is only when we have both mutable and immutable array variants 
with the same storage rep, in the same world, that we can support 
doing freeze and thaw on arrays.

the reason why this is separate from the the Array and MArray instances
is that we can't (in general) assume that every immutable array type 
has a corresponding mutable one. 

NB, however, could we always assume that if we have a mutable array type,
that theres always a corresponding immutable type?

-}