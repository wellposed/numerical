

module PropLayout where


layoutProperties = undefined 

{-
a basic property that is always true (for dense formats):

for all valid Layout Forms FRM with shape SHP,
    for all index ix with 0≤ix≤SHP,
        ix == toIndex FRM $! toAddress FRM ix

ie, id_Index == toIndex FRM . toAddress FRM

the ``dual'', 
    id_{Address} == toAddress FRM . toIndex FRM
doesn't hold unless the address space is contiguous/the address is valid 
(basically it gets subtle)

Unless the layout format (Form) is equipped with a striding model
such that the "remainder/residual" after doing the stride adjustments is 0

in some respects, Sparse Formats are sort of a latent sibling to contiguous 
formats, at least in the higher dimensional general case, but only
for strictly contiguous respecting slices... theres some 

-}

