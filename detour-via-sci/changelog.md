# 1.0.1

No code changes, added doctests of fromSci and showSci for -0.038.

# 1.0.0

Initial version with;

* `toSci` and `fromSci` for converting Rational to and from Scientific.
* `showSci` for showing Rational as if Scientific.
* For JSON, instances of `ToJSON` and `FromJSON` for newtype Rational that also
  have instances of `DefaultDecimalPlaces` and `NewType`.
* For CSV, instances of `ToField` and `FromField` in the same way.
* Template Haskell functions to generate instances.
