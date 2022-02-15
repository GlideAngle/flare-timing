let mkHome = ./../home.dhall

in  let defs = ./../defaults.dhall
    
    in    defs
        ⫽ ./../default-extensions.dhall
        ⫽ { name =
              "flight-route"
          , homepage =
              mkHome "lang-haskell/route#readme"
          , synopsis =
              "Control zones to fly."
          , description =
              "Control zones for hang gliding and paragliding competitons."
          , category =
              "Flight"
          , ghc-options =
              [ "-Wall", "-fplugin Data.UnitsOfMeasure.Plugin" ]
          , dependencies =
                defs.dependencies
              # [ "numbers"
                , "uom-plugin"
                , "bifunctors"
                , "aeson"
                , "scientific"
                , "hcoord-utm"
                , "detour-via-sci"
                , "siggy-chardust"
                , "flight-earth"
                , "flight-latlng"
                , "flight-task"
                , "flight-units"
                , "flight-zone"
                ]
          , library =
              { source-dirs =
                  "library"
              , exposed-modules =
                  [ "Flight.Route"
                  , "Flight.TaskTrack.Double"
                  , "Flight.TaskTrack.Rational"
                  ]
              }
          , tests =
              ./../default-tests.dhall
          }
