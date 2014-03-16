#note 

this library is pre release and not ready for general circulation or use.
that will change soon though

# About  Numerical-Core
This is the types package for Numerical Haskell, a project by Carter Schonwald,
Wellposed Ltd, and other contributors.

##Numerical Haskell
Numerical Haskell is an effort to bring great numerical computation and data analysis
tooling to 



# Contributing 
Great! Theres so many awesome ways you could help out. Look at CONTRIBUTING.md for more details


# FAQ
1. How do I use Numerical haskell to write fast code thats outstandingly high level !?
    * The leading cause of poor performance in numerical routines is bad memory locality,
    which has but a single easy cure: ** block recursive algorithms **
    * Yes, you heard me, in compiled languages recursion is pretty cheap outside of the inner
    most loops! It also is a fantastic tool for facilitating good memory locality!
    * I'm totally serious, try out the benchmarks for the various versions of the same routines we
    provide!
2. But, what about fusion?   
    * Because of certain aspects of the numerical haskell design, we can't *automagically* use
    the fusion optimization facilities of the underlying array representations such as Vector.
    










