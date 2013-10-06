{-# LANGUAGE PolyKinds   #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Numerics.Types.Array where


data  MArray  world rep lay (view:: Locality) sh elem 



 


{-
lets do just IO and not ST for now?
or bite the primstate bullet now?
-}

data family Array world rep lay (view:: Locality) sh elm 


-----------
-- | for now locality is a closed type, may change going forward
data  Locality = Contiguous | Strided 


{-
theres several points in the design space of array apis that are nice, but none quite right

Vector is probably the closest
pros:
  which has nice pure vs mutable apis
  simple interface
cons: 
  its really designed for Int indexing
  assumes every pure Vector is internally derived from an imperative one
    (this is reflected  in where the thaw/freeze)

so there needs to be an Array class, an MArray class,

and the Thawing / Freezing needs to be in a seperate PhasedArray class!
why? Because we can't assume that pure/mutable arrays are the fundamental data type!

-}


{-
maybe do 
     data Locality = Contiguous | Strided

For now lets assume that the concrete (rather than delayed) arrays
have a regular structure when strided. (rather than nonuniform gaps)
-}
{-
rep = storable, unboxed, boxed, delay, etc

lay = row major, column major, morton z, morton w (flipped n),
  --- this  ignores symmetry  and hermitian being properties as well as packed layouts
  --- also need to have a good sparse story
    --- as currently done, most don't really make sense for != rank-2 arrays, 

-- rowMajor is a foldR, columnMajor is a foldL  over the shape ices  

-- Repa and accelerate use a Snoc List so that Row major fuses well for row major
    
sh= rank / shape, ie matrix or vector, or some  higher tensor thingy
lets borrow from  repa/ accelea


mode= need to have a notion of runnable worlds, 
based on "backend" chosen, eg CBlasish, DPH, Repa, LLVM, Free (get the shallow/ deep ast)

view =  
    Origin, Slice, and Diced, I might make this a fixed universe for now
    Lets not distinguish whether a contiguous array is the original or derived for now
    doesn't seem to be a meaningful difference and would make type inference crap / not bijective
    Note that this does mean that accidental space leaks may happen

    that suggests (but not for now) having a notion of origin / derived
    that would allow elimiting space leaks. But lets not do that for now

-}

{- what class
-}


