[![Wellposed](http://www.wellposed.com/mini.png)](http://www.wellposed.com)™

# Currently Experimental


# About  Numerical-Core
This is the core Package for Numerical Haskell, a project by Carter Schonwald aka
Wellposed Ltd, and (soon I hope!) other contributors.

Numerical-Core is an open source component of the [Wellposed](http://www.wellposed.com)® Numerical Haskell software suite.

## How to use
This library is a technology demonstrator for mechanisms to simplify writing
arbitrary 

##Build Status

[![Build Status](https://secure.travis-ci.org/wellposed/numerical.png?branch=master)](http://travis-ci.org/wellposed/numerical)


#note

this library is **pre alpha release** so not all examples / codes may work as expected.
That said, the current api should be enough to prototype and typecheck algorithms.



# Numerical Haskell
Numerical Haskell is an effort to bring great numerical computation and data analysis
tooling to haskell, and be the best possible platform for sophisticated efforts in those same domains

## What array Formats are Supported

The initial

## What convention is used for indexing?

When you have an index tuple, just think  ``x,y,z``  to keep track of the meaning.
Indexing tuples are written as statically sized lists, eg ``x:*y:*z:*Nil``.
This follows the tradition of x,y,z axes used in plotting. Note well: the underlying memory
order can be row OR column major or other!


All the computations on these static sized lists get specialized away into
nonrecursive computations at their use sites. So in this special scenario, lists aren't a problem!



# Contributing
Great! Theres so many awesome ways you could help out. Look at CONTRIBUTING.md for more details.
Right now theres a lot of low hanging fruit in improving test coverage,
and soon there'll be many opportunities on the performance tuning and numerical
algorithms/tooling areas.

## bug reports
see bug.md for how to file a bug report


# Performance FAQ
1. How do I use Numerical haskell to write fast code thats outstandingly high level !?
    * The leading cause of poor performance in numerical routines (aside from poor choice
    in algorithms) is bad memory locality,
    which has but a single easy cure: ** block recursive algorithms **
    * Yes, you heard me, in compiled languages recursion is pretty cheap outside of the inner
    most loops! It also is a fantastic tool for facilitating good memory locality!
    * I'm totally serious, try out the benchmarks for the various versions of the same routines we
    provide!
2. But, what about fusion?
    * Because of certain aspects of the numerical haskell design, we can't *automagically* use
    the fusion optimization facilities of the underlying array representations such as Vector.

# Community
Many member of the Numerical Haskell community can be found on `#numerical-haskell` on freenode IRC
There is also  the [numericalhaskell mailing list](https://groups.google.com/forum/#!forum/numericalhaskell)

# Support
The community provides some basic support through the IRC channel, Mailing list,
and the relevant project [issue trackers](http://github.com/wellposed).

If your support needs can't be resolved though those channels, please do not
hesistate to contact Wellposed (aka Carter) to find out more about our support and
professional services options.
















