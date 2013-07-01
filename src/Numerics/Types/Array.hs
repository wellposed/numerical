{-# LANGUAGE PolyKinds   #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Numerics.Types.Array where


data family ArrayM st mode rep lay (view:: View) sh elm 

data family Array mode rep lay (view:: View) sh elm 

data View = Origin | Slice | Dice 

{-
rep = storable, unboxed, boxed, delay, etc

lay = row major, column major, morton z, morton w (flipped n),
  --- this  ignores symmetry  and hermitian being properties as well as packed layouts
  --- also need to have a good sparse story
    --- as currently done, most don't really make sense for != rank-2 arrays, 

-- rowMajor is a foldR, columnMajor is a foldL    

-- Repa and accelerate use a Snoc List so that Row major fuses well
    
sh= rank / shape, ie matrix or vector, or some  higher tensor thingy


mode= need to have a notion of runnable worlds, 
based on "backend" chosen, eg CBlasish, DPH, Repa, LLVM, Free (get the shallow/ deep ast)

view =  Origin, Slice, and Diced, I might make this a fixed universe for now


-}


