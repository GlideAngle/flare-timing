let mkHome = ./../home.dhall

in  let defs =
          ./../defaults.dhall
    
    in  let exts =
              ./../default-extensions.dhall
        
        in    defs
            ⫽ { name =
                  "flight-mask"
              , homepage =
                  mkHome "lang-haskell/mask#readme"
              , synopsis =
                  "Track logs masked by competition task zones."
              , description =
                  "Masking tracks with zones, work out; did the pilot launch, did they make goal and how long did that take? What was distance to goal?"
              , category =
                  "Data"
              , extra-source-files =
                  defs.extra-source-files # [ "**/*.yaml" ]
              , ghc-options =
                  [ "-Wall", "-fplugin Data.UnitsOfMeasure.Plugin" ]
              , default-extensions =
                    exts.default-extensions
                  # [ "AllowAmbiguousTypes"
                    , "InstanceSigs"
                    , "UndecidableSuperClasses"
                    ]
              , dependencies =
                    defs.dependencies
                  # [ "bytestring"
                    , "cmdargs"
                    , "containers"
                    , "deepseq"
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
                    , "flight-gap-allot"
                    , "flight-gap-lead"
                    , "flight-kml"
                    , "flight-latlng"
                    , "flight-route"
                    , "flight-scribe"
                    , "flight-span"
                    , "flight-task"
                    , "flight-track"
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
                      , "Flight.Comp.Distance.Double"
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
                            [ "doctest", "utf8-string" ]
                        , ghc-options =
                            [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
                        , main =
                            "DocTest.hs"
                        , source-dirs =
                            [ "library", "test-suite-doctest" ]
                        }
                    }
              }
