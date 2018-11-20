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
          # [ "split"
            , "path"
            , "containers"
            , "mtl"
            , "directory"
            , "filepath"
            , "time"
            , "lens"
            , "bytestring"
            , "yaml"
            , "uom-plugin"
            , "fgl"
            , "cmdargs"
            , "numbers"
            , "detour-via-sci"
            , "flight-span"
            , "flight-units"
            , "flight-comp"
            , "flight-kml"
            , "flight-latlng"
            , "flight-zone"
            , "flight-route"
            , "flight-track"
            , "flight-earth"
            , "flight-task"
            , "flight-gap"
            , "flight-scribe"
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
              ]
          }
      , tests =
          ./../default-tests.dhall
      }
