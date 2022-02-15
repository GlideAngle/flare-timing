let mkHome = ./../home.dhall

in  let defs = ./../defaults.dhall
    
    in    defs
        ⫽ ./../default-extensions.dhall
        ⫽ { name =
              "flight-span"
          , homepage =
              mkHome "lang-haskell/span#readme"
          , synopsis =
              "How to measure a distance that spans two points."
          , description =
              "How to measure a distance that spans tow points."
          , category =
              "Data"
          , dependencies =
              defs.dependencies # [ "cmdargs" ]
          , library =
              { source-dirs = "library", exposed-modules = "Flight.Span.Math" }
          , tests =
              ./../default-tests.dhall
          }
