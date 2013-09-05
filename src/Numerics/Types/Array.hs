{-# LANGUAGE PolyKinds   #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Numerics.Types.Array where



data family ArrayM  world rep lay (view:: Locality) sh elm 
{-
lets do just IO and not ST for now
-}

data family Array world rep lay (view:: Locality) sh elm 

data Locality = Contiguous | Strided 



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


