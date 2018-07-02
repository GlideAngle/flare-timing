    let defs = ./defaults.dhall 

in    defs
    ⫽ { name =
          "detour-via-sci"
      , version =
          "1.0.0"
      , synopsis =
          "JSON and CSV encoding for rationals as decimal point numbers."
      , description =
          "Lossy JSON and CSV encoding and decoding for newtype rationals via scientific with fixed decimal places."
      , category =
          "Data, Math, Numeric, JSON, CSV"
      , github =
          "blockscope/flare-timing/detour-via-sci"
      , homepage =
          "https://github.com/blockscope/flare-timing/tree/master/detour-via-sci#readme"
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
          { hlint =
              { dependencies =
                  [ "hlint" ]
              , ghc-options =
                  [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
              , main =
                  "HLint.hs"
              , source-dirs =
                  [ "library", "test-suite-hlint" ]
              }
          , doctest =
              { dependencies =
                  [ "doctest" ]
              , ghc-options =
                  [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
              , main =
                  "DocTest.hs"
              , source-dirs =
                  [ "library", "test-suite-doctest" ]
              }
          }
      }
