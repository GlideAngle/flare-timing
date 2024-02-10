\(_stackage-resolver : Optional Text) ->
  ''
  user-message: "WARNING: This stack project is generated."

  ghc-options:
      "$locals": -Werror=unused-imports

  flags:
      detour-via-sci:
          suppress-failing-tests: true
      detour-via-uom:
          suppress-failing-tests: true
      flight-clip:
          suppress-failing-tests: true
      flight-comp:
          suppress-failing-tests: true
      flight-earth:
          suppress-failing-tests: true
      flight-fsdb:
          suppress-failing-tests: true
      flight-gap-allot:
          suppress-failing-tests: false
      flight-gap-effort:
          suppress-failing-tests: false
      flight-gap-lead:
          suppress-failing-tests: true
      flight-gap-math:
          suppress-failing-tests: false
      flight-gap-stop:
          suppress-failing-tests: false
      flight-gap-valid:
          suppress-failing-tests: false
      flight-gap-weight:
          suppress-failing-tests: true
      flight-gap:
          suppress-failing-tests: true
      flight-igc:
          suppress-failing-tests: true
      flight-kml:
          suppress-failing-tests: true
      flight-mask:
          suppress-failing-tests: true
      flight-task:
          suppress-failing-tests: true
      flight-time:
          suppress-failing-tests: true
      flight-track:
          suppress-failing-tests: true
      flight-units:
          suppress-failing-tests: false
      flight-zone:
          suppress-failing-tests: true
      flare-timing:
          suppress-test-parsers: true
  ''
