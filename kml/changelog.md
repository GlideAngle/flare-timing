# 1.0.1

* The doctest was unable to find `test-suite-doctest/*.kml` files needed for
  testing. With a `**/*.kml` pattern, these are added to the list of
  `extra-source-files`.

# 1.0.0

Initial version with;

* Data types for a single fix. This being made up of a time stamp, latitude,
  longitude and GPS altitude with an optional barometric pressure altitude.
* Data types for tracklogs made up of a series of fixes.
* A `parse` function.
