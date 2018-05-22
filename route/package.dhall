  ./defaults.dhall 
⫽ { name =
      "flight-route"
  , synopsis =
      "Control zones to fly."
  , description =
      "Control zones for hang gliding and paragliding competitons."
  , category =
      "Flight"
  , github =
      "blockscope/flare-timing/route"
  , ghc-options =
      [ "-Wall", "-fplugin Data.UnitsOfMeasure.Plugin" ]
  , library =
      { dependencies =
          [ "base >=4.5 && <5"
          , "numbers"
          , "uom-plugin"
          , "bifunctors"
          , "aeson"
          , "scientific"
          , "hcoord-utm"
          , "aeson-via-sci"
          , "siggy-chardust"
          , "flight-units"
          , "flight-latlng"
          , "flight-zone"
          , "flight-earth"
          , "flight-task"
          ]
      , source-dirs =
          "library"
      , exposed-modules =
          [ "Flight.Route"
          , "Flight.TaskTrack.Double"
          , "Flight.TaskTrack.Rational"
          ]
      }
  , tests =
      { hlint =
          { dependencies =
              [ "base"
              , "numbers"
              , "hlint"
              , "uom-plugin"
              , "bifunctors"
              , "aeson"
              , "scientific"
              , "hcoord-utm"
              , "aeson-via-sci"
              , "siggy-chardust"
              , "flight-units"
              , "flight-latlng"
              , "flight-zone"
              , "flight-earth"
              , "flight-task"
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
