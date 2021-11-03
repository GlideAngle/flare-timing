let mkHome = ./../home.dhall

in  let defs =
          ./../defaults.dhall
    
    in    defs
        ⫽ { name =
              "detour-via-sci"
          , homepage =
              mkHome "lang-haskell/detour-via-sci#readme"
          , version =
              "1.0.1"
          , synopsis =
              "JSON and CSV encoding for rationals as decimal point numbers."
          , description =
              "Lossy JSON and CSV encoding and decoding for newtype rationals via scientific with fixed decimal places."
          , category =
              "Data, Math, Numeric, JSON, CSV"
          , dependencies =
                defs.dependencies
              # [ "newtype"
                , "scientific"
                , "aeson"
                , "cassava"
                , "template-haskell"
                , "siggy-chardust"
                ]
          , library =
              { source-dirs =
                  "library"
              , exposed-modules =
                  [ "Data.Via.Scientific" ]
              }
          , tests =
                ./../default-tests.dhall
              ⫽ { doctest =
                    { dependencies =
                        [ "doctest", "text", "vector" ]
                    , ghc-options =
                        [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
                    , main =
                        "DocTest.hs"
                    , source-dirs =
                        [ "library", "test-suite-doctest" ]
                    }
                }
          }
