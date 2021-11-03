let mkHome = ./../home.dhall

in  let defs =
          ./../defaults.dhall
    
    in    defs
        ⫽ ./../default-extensions.dhall
        ⫽ { name =
              "flight-kml"
          , homepage =
              mkHome "lang-haskell/kml#readme"
          , version =
              "1.1.0"
          , synopsis =
              "Parsing of pilot tracklogs dumped as KML."
          , description =
              "Provides parsing of dumped tracklogs. In hang gliding and paragliding competitions when FS and GpsDump are paired in competition mode a pilot's tracklog is dumped as KML. This is exlained in detail on the FS wiki."
          , category =
              "Data, Parsing, Geography, Gps, Flight, XML, KML"
          , extra-source-files =
              defs.extra-source-files # [ "**/*.kml" ]
          , dependencies =
                defs.dependencies
              # [ "split"
                , "deepseq"
                , "megaparsec"
                , "hxt"
                , "path"
                , "hxt-xpath"
                , "aeson"
                , "time"
                , "detour-via-sci"
                , "siggy-chardust"
                , "flight-clip"
                ]
          , library =
              { source-dirs =
                  "library"
              , exposed-modules =
                  [ "Flight.Kml", "Flight.Kml.Internal" ]
              }
          , tests =
                ./../default-tests.dhall
              ⫽ { doctest =
                    { dependencies =
                        [ "template-haskell", "doctest" ]
                    , ghc-options =
                        [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
                    , main =
                        "DocTest.hs"
                    , source-dirs =
                        [ "library", "test-suite-doctest" ]
                    }
                , parse =
                    { dependencies =
                        [ "base"
                        , "split"
                        , "megaparsec"
                        , "hxt"
                        , "path"
                        , "hxt-xpath"
                        , "aeson"
                        , "time"
                        , "raw-strings-qq"
                        , "tasty"
                        , "tasty-hunit"
                        , "tasty-quickcheck"
                        , "tasty-smallcheck"
                        , "smallcheck"
                        , "detour-via-sci"
                        , "siggy-chardust"
                        ]
                    , ghc-options =
                        [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
                    , main =
                        "Parse.hs"
                    , source-dirs =
                        [ "library", "test-suite-parse" ]
                    }
                }
          }
