  ./defaults.dhall 
⫽ { name =
      "detour-via-uom"
  , version =
      "1.0.0"
  , synopsis =
      "JSON encoding and decoding for rationals via scientific."
  , description =
      "Lossy JSON encoding for rationals."
  , category =
      "Flight"
  , github =
      "blockscope/flare-timing/detour-via-uom"
  , ghc-options =
      [ "-Wall", "-fplugin Data.UnitsOfMeasure.Plugin" ]
  , library =
      { dependencies =
          [ "base >=4.5 && <5"
          , "newtype"
          , "scientific"
          , "aeson"
          , "uom-plugin"
          , "detour-via-sci"
          ]
      , source-dirs =
          "library"
      , exposed-modules =
          [ "Data.Via.UnitsOfMeasure" ]
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
              , "detour-via-sci"
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
