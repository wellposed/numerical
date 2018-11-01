
# How to submit a bug report

If you're just reporting a bug, thats great! Please be sure to
provide a minimal example that can reproduce the error. Please note any
examples must be under a BSD2/MIT compatible license so that each new
test case can be used to augment the test suite.

 1. Any bug report should strive to provide a minimal self contained test case,
ideally cabalized for easy reproducibility. This will greatly speed up the
turn around for any applicable bug fixes.
If possible/applicabe please also include: your OS version, GHC version, the output of
`GHC --info`, what versions of the numerical-haskell libs you have installed,
what versions of the dependencies are installed, and also the relevant versions

 2. For bug reports regarding incorrect answers for numerical algorithms,
please give both an example input that can reproduce the incorrect result,
and at the very minimum an unoptimized reference routine that will give
the correct answer.

Note that before reporting a bug of type (2), please check that
the problem input is **well-conditioned**. Algorithms giving bad answers on
**ill-conditioned** inputs is a problem in numerical computing even if you have
exact arithmetic!  Most algorithms should provide a means of estimating
their condition number and/or rate of convergence.

Ill-conditioned computations often benefit from reformulating how you solve the
problem! (Which isn't always easy :) )

Numerical Computing is a very rich and deep problem domain, and
Numerical Haskell can't hope to give you the answer for every problem,
merely the tools to help you build your own solutions. If you hit such a wall,
please reach out and help us figure out how to help you!
