  ./defaults.dhall 
⫽ { name =
      "aeson-via-uom"
  , synopsis =
      "JSON encoding and decoding for rationals via scientific."
  , description =
      "Lossy JSON encoding for rationals."
  , category =
      "Flight"
  , github =
      "blockscope/flare-timing/aeson-via-uom"
  , ghc-options =
      [ "-Wall", "-fplugin Data.UnitsOfMeasure.Plugin" ]
  , library =
      { dependencies =
          [ "base >=4.5 && <5"
          , "newtype"
          , "scientific"
          , "aeson"
          , "uom-plugin"
          , "aeson-via-sci"
          , "flight-units"
          ]
      , source-dirs =
          "library"
      , exposed-modules =
          [ "Data.Aeson.Via.UnitsOfMeasure" ]
      }
  , tests =
      { hlint =
          { dependencies =
              [ "base"
              , "hlint"
              , "newtype"
              , "scientific"
              , "aeson"
              , "uom-plugin"
              , "aeson-via-sci"
              , "flight-units"
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
