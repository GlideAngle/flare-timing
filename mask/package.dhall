    let defs = ./../defaults.dhall

in    defs
    ⫽ ./../default-extensions.dhall
    ⫽ { name =
          "flight-mask"
      , synopsis =
          "Track logs masked by competition task zones."
      , description =
          "Masking tracks with zones, work out; did the pilot launch, did they make goal and how long did that take? What was distance to goal?"
      , category =
          "Data"
      , github =
          "blockscope/flare-timing/mask"
      , ghc-options =
          [ "-Wall", "-fplugin Data.UnitsOfMeasure.Plugin" ]
      , dependencies =
            defs.dependencies
          # [ "bytestring"
            , "cmdargs"
            , "containers"
            , "directory"
            , "filepath"
            , "fgl"
            , "lens"
            , "mtl"
            , "numbers"
            , "path"
            , "safe-exceptions"
            , "split"
            , "these"
            , "time"
            , "uom-plugin"
            , "yaml"
            , "flight-clip"
            , "flight-comp"
            , "flight-earth"
            , "flight-gap"
            , "flight-kml"
            , "flight-latlng"
            , "flight-route"
            , "flight-scribe"
            , "flight-span"
            , "flight-track"
            , "flight-task"
            , "flight-units"
            , "flight-zone"
            , "detour-via-sci"
            , "siggy-chardust"
            ]
      , library =
          { source-dirs =
              "library"
          , exposed-modules =
              [ "Flight.Mask"
              , "Flight.Comp.Distance"
              , "Flight.Span.Double"
              , "Flight.Span.Rational"
              , "Flight.Mask.Internal.Race"
              , "Flight.Mask.Internal.Zone"
              , "Flight.Mask.Tag"
              ]
          }
      , tests =
            ./../default-tests.dhall
          ⫽ { doctest =
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
