# 1.0.1

Add a failing doctest roundtripping encoding and decoding with -0.038 km and
fix this by reimplementing `showQuantity` from the `uom-plugin` using `showSci`
and the known decimal places. Showing quantities with decimal places this way
fixes the roundtripping problem of `ToJSON` paired with `FromJSON` and
`ToField` paired with `FromField`.

# 1.0.0

Initial version with;

* For JSON, instances of `ToJSON` and `FromJSON` for newtype quantities that
  also have instances of `DefaultDecimalPlaces` and `NewType`.
* For CSV, instances of `ToField` and `FromField` in the same way.
