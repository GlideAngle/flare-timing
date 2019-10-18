# `siggy-chardust`

[![hackage release](https://img.shields.io/hackage/v/siggy-chardust.svg?label=hackage)](http://hackage.haskell.org/package/siggy-chardust)
[![Dependencies of latest version on Hackage](https://img.shields.io/hackage-deps/v/siggy-chardust.svg)](https://hackage.haskell.org/package/siggy-chardust)
[![stackage LTS package](http://stackage.org/package/siggy-chardust/badge/lts)](http://stackage.org/lts/package/siggy-chardust)
[![stackage Nightly package](http://stackage.org/package/siggy-chardust/badge/nightly)](http://stackage.org/nightly/package/siggy-chardust)

Rounding rationals to significant digits and decimal places.

The 'round' function from the prelude returns an integer. The standard librarys
of C and C++ have round functions that return floating point numbers. Rounding
in this library takes and returns 'Rational's and can round to a number of
significant digits or a number of decimal places.
