let mkHome = ./../home.dhall

in  let defs =
          ./../defaults.dhall
    
    in    defs
        ⫽ ./../default-extensions.dhall
        ⫽ { name =
              "flight-time"
          , homepage =
              mkHome "lang-haskell/time#readme"
          , synopsis =
              "Align times of competing pilot's tracklogs."
          , description =
              "From a defined start, align the times of pilots on track so that we can then work out the leading points."
          , category =
              "Data"
          , ghc-options =
              [ "-Wall", "-fplugin Data.UnitsOfMeasure.Plugin" ]
          , dependencies =
                defs.dependencies
              # [ "directory"
                , "filepath"
                , "lens"
                , "mtl"
                , "safe-exceptions"
                , "these"
                , "time"
                , "uom-plugin"
                , "flight-clip"
                , "flight-zone"
                , "flight-earth"
                , "flight-comp"
                , "flight-kml"
                , "flight-latlng"
                , "flight-lookup"
                , "flight-mask"
                , "flight-scribe"
                , "flight-span"
                , "siggy-chardust"
                ]
          , library =
              { source-dirs =
                  "library"
              , exposed-modules =
                  [ "Flight.Time.Align" ]
              }
          , tests =
                ./../default-tests.dhall
              ⫽ { golden =
                    { main =
                        "Golden.hs"
                    , source-dirs =
                        [ "test/golden/src" ]
                    , dependencies =
                        [ "base"
                        , "Cabal"
                        , "Diff"
                        , "filepath"
                        , "microlens"
                        , "prettyprinter"
                        , "tasty"
                        , "tasty-golden"
                        , "text"
                        , "transformers"
                        , "aeson"
                        , "utf8-string"
                        , "directory"
                        , "flight-time"
                        , "flight-kml"
                        , "vector"
                        ]
                    , when =
                        { condition =
                            "flag(suppress-failing-tests)"
                        , buildable =
                            False
                        }
                    }
                }
          }
