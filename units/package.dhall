  ./defaults.dhall 
⫽ { name =
      "flight-units"
  , synopsis =
      "Units used in hang gliding and paragliding competitions."
  , description =
      "Unit definitions such as m, km, rad and deg."
  , category =
      "Flight"
  , github =
      "blockscope/flare-timing/units"
  , ghc-options =
      [ "-Wall", "-fplugin Data.UnitsOfMeasure.Plugin" ]
  , library =
      { dependencies =
          [ "base >=4.5 && <5"
          , "numbers"
          , "fixed"
          , "bifunctors"
          , "text"
          , "formatting"
          , "uom-plugin"
          , "siggy-chardust"
          ]
      , source-dirs =
          "library"
      , exposed-modules =
          [ "Flight.Ratio"
          , "Flight.Units"
          , "Flight.Units.Angle"
          , "Flight.Units.DegMinSec"
          ]
      }
  , tests =
      { hlint =
          { dependencies =
              [ "base"
              , "hlint"
              , "numbers"
              , "fixed"
              , "bifunctors"
              , "text"
              , "formatting"
              , "uom-plugin"
              , "siggy-chardust"
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
