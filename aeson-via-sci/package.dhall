  ./defaults.dhall 
⫽ { name =
      "aeson-via-sci"
  , synopsis =
      "JSON encoding and decoding for rationals via scientific."
  , description =
      "Lossy JSON encoding for rationals."
  , category =
      "Flight"
  , github =
      "blockscope/flare-timing/aeson-via-sci"
  , library =
      { dependencies =
          [ "base >=4.5 && <5"
          , "newtype"
          , "scientific"
          , "aeson"
          , "cassava"
          , "template-haskell"
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
              ]
          , ghc-options =
              [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
          , main =
              "HLint.hs"
          , source-dirs =
              [ "library", "test-suite-hlint" ]
          }
      }
  }
