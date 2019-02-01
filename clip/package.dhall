    let defs = ./../defaults.dhall

in    defs
    ⫽ ./../default-extensions.dhall
    ⫽ { name =
          "flight-clip"
      , version =
          "1.1.0"
      , synopsis =
          "Clipping a pilot's tracklogs."
      , description =
          "Provides types and typeclasses for clipping a tracklog."
      , category =
          "Data, Parsing, Flight"
      , github =
          "blockscope/flare-timing/clip"
      , homepage =
          "https://github.com/blockscope/flare-timing/tree/master/clip#readme"
      , dependencies =
            defs.dependencies
          # [ "time" ]
      , library =
          { source-dirs =
              "library"
          , exposed-modules =
              [ "Flight.Clip" ]
          }
      , tests = ./../default-tests.dhall
      }
