  ./defaults.dhall 
⫽ { name =
      "detour-via-sci"
  , version =
      "1.0.0"
  , synopsis =
      "Lossy JSON and CSV encoding for rationals as decimal point numbers."
  , description =
      "JSON and CSV encoding and decoding for newtype rationals via scientific with fixed decimal places."
  , category =
      "data, math, numeric, json, csv"
  , github =
      "blockscope/flare-timing/detour-via-sci"
  , library =
      { dependencies =
          [ "base >=4.5 && <5"
          , "newtype"
          , "scientific"
          , "aeson"
          , "cassava"
          , "template-haskell"
          , "siggy-chardust"
          ]
      , source-dirs =
          "library"
      , exposed-modules =
          [ "Data.Via.Scientific" ]
      }
  , tests =
      { hlint =
          { dependencies =
              [ "base"
              , "hlint"
              , "newtype"
              , "scientific"
              , "aeson"
              , "cassava"
              , "template-haskell"
              , "siggy-chardust"
              ]
          , ghc-options =
              [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
          , main =
              "HLint.hs"
          , source-dirs =
              [ "library", "test-suite-hlint" ]
          }
      , doctest =
          { dependencies =
              [ "base"
              , "newtype"
              , "scientific"
              , "aeson"
              , "cassava"
              , "template-haskell"
              , "siggy-chardust"
              , "doctest"
              ]
          , ghc-options =
              [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
          , main =
              "DocTest.hs"
          , source-dirs =
              [ "library", "test-suite-doctest" ]
          }
      }
  }
