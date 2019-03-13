    let defs = ./../defaults.dhall

in  let depsTest =
            defs.dependencies
          # [ "directory"
            , "filepath"
            , "filemanip"
            , "raw-strings-qq"
            , "cmdargs"
            , "flight-cmd"
            , "flight-comp"
            ]

in  let deps =
            defs.dependencies
          # [ "directory"
            , "filepath"
            , "filemanip"
            , "raw-strings-qq"
            , "cmdargs"
            , "mtl"
            , "transformers"
            , "yaml"
            , "aeson"
            , "uom-plugin"
            , "bytestring"
            , "clock"
            , "formatting"
            , "flight-cmd"
            , "flight-comp"
            , "flight-latlng"
            , "flight-scribe"
            , "flight-time"
            ]

in    defs
    ⫽ ./../default-extensions.dhall
    ⫽ { name =
          "flare-timing"
      , synopsis =
          "A collection of apps and libraries for scoring hang gliding and paragliding competitions."
      , description =
          "Scoring and viewing hang gliding and paragliding competitions."
      , category =
          "Data, Parsing"
      , github =
          "blockscope/flare-timing/flare-timing"
      , executables =
          { extract-input =
              { dependencies =
                    deps
                  # [ "flight-earth"
                    , "flight-fsdb"
                    , "flight-gap"
                    , "flight-mask"
                    , "flight-zone"
                    ]
              , ghc-options =
                  [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
              , main =
                  "ExtractInputMain.hs"
              , source-dirs =
                  "prod-apps/extract-input"
              }
          , fs-score =
              { dependencies =
                    deps
                  # [ "flight-earth"
                    , "flight-fsdb"
                    , "flight-gap"
                    , "flight-mask"
                    , "flight-zone"
                    ]
              , ghc-options =
                  [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
              , main =
                  "FsScoreMain.hs"
              , source-dirs =
                  "prod-apps/fs-score"
              }
          , task-length =
              { dependencies =
                  deps # [ "safe-exceptions", "flight-route" ]
              , ghc-options =
                  [ "-rtsopts"
                  , "-threaded"
                  , "-with-rtsopts=-N"
                  , "-Wall"
                  , "-fplugin Data.UnitsOfMeasure.Plugin"
                  ]
              , main =
                  "TaskLengthMain.hs"
              , source-dirs =
                  "prod-apps/task-length"
              }
          , cross-zone =
              { dependencies =
                    deps
                  # [ "lens"
                    , "siggy-chardust"
                    , "safe-exceptions"
                    , "flight-span"
                    , "flight-units"
                    , "flight-zone"
                    , "flight-earth"
                    , "flight-mask"
                    ]
              , ghc-options =
                  [ "-rtsopts"
                  , "-threaded"
                  , "-with-rtsopts=-N"
                  , "-Wall"
                  , "-fplugin Data.UnitsOfMeasure.Plugin"
                  ]
              , main =
                  "CrossZoneMain.hs"
              , source-dirs =
                  "prod-apps/cross-zone"
              }
          , tag-zone =
              { dependencies =
                    deps
                  # [ "time", "safe-exceptions", "flight-mask", "flight-zone" ]
              , ghc-options =
                  [ "-rtsopts"
                  , "-threaded"
                  , "-with-rtsopts=-N"
                  , "-Wall"
                  , "-fplugin Data.UnitsOfMeasure.Plugin"
                  ]
              , main =
                  "TagZoneMain.hs"
              , source-dirs =
                  "prod-apps/tag-zone"
              }
          , unpack-track =
              { dependencies =
                    deps
                  # [ "lens"
                    , "time"
                    , "siggy-chardust"
                    , "safe-exceptions"
                    , "flight-kml"
                    , "flight-mask"
                    , "flight-lookup"
                    ]
              , ghc-options =
                  [ "-rtsopts"
                  , "-threaded"
                  , "-with-rtsopts=-N"
                  , "-Wall"
                  , "-fplugin Data.UnitsOfMeasure.Plugin"
                  ]
              , main =
                  "UnpackTrackMain.hs"
              , source-dirs =
                  "prod-apps/unpack-track"
              }
          , align-time =
              { dependencies =
                    deps
                  # [ "lens"
                    , "time"
                    , "siggy-chardust"
                    , "safe-exceptions"
                    , "flight-kml"
                    , "flight-mask"
                    , "flight-lookup"
                    ]
              , ghc-options =
                  [ "-rtsopts"
                  , "-threaded"
                  , "-with-rtsopts=-N"
                  , "-Wall"
                  , "-fplugin Data.UnitsOfMeasure.Plugin"
                  ]
              , main =
                  "AlignTimeMain.hs"
              , source-dirs =
                  "prod-apps/align-time"
              }
          , discard-further =
              { dependencies =
                    deps
                  # [ "safe-exceptions"
                    , "flight-clip"
                    , "flight-route"
                    , "flight-gap"
                    , "flight-mask"
                    , "flight-lookup"
                    ]
              , ghc-options =
                  [ "-rtsopts"
                  , "-threaded"
                  , "-with-rtsopts=-N"
                  , "-Wall"
                  , "-fplugin Data.UnitsOfMeasure.Plugin"
                  ]
              , main =
                  "DiscardFurtherMain.hs"
              , source-dirs =
                  "prod-apps/discard-further"
              }
          , mask-track =
              { dependencies =
                    deps
                  # [ "containers"
                    , "lens"
                    , "siggy-chardust"
                    , "safe-exceptions"
                    , "time"
                    , "flight-clip"
                    , "flight-gap"
                    , "flight-kml"
                    , "flight-lookup"
                    , "flight-mask"
                    , "flight-route"
                    , "flight-span"
                    ]
              , ghc-options =
                  [ "-rtsopts"
                  , "-threaded"
                  , "-with-rtsopts=-N"
                  , "-Wall"
                  , "-fplugin Data.UnitsOfMeasure.Plugin"
                  ]
              , main =
                  "MaskTrackMain.hs"
              , source-dirs =
                  "prod-apps/mask-track"
              }
          , land-out =
              { dependencies =
                  deps # [ "safe-exceptions", "flight-gap" ]
              , ghc-options =
                  [ "-rtsopts"
                  , "-threaded"
                  , "-with-rtsopts=-N"
                  , "-Wall"
                  , "-fplugin Data.UnitsOfMeasure.Plugin"
                  ]
              , main =
                  "LandOutMain.hs"
              , source-dirs =
                  "prod-apps/land-out"
              }
          , gap-point =
              { dependencies =
                    deps
                  # [ "containers"
                    , "safe-exceptions"
                    , "flight-zone"
                    , "flight-gap"
                    , "flight-lookup"
                    , "flight-mask"
                    , "flight-route"
                    ]
              , ghc-options =
                  [ "-rtsopts"
                  , "-threaded"
                  , "-with-rtsopts=-N"
                  , "-Wall"
                  , "-fplugin Data.UnitsOfMeasure.Plugin"
                  ]
              , main =
                  "GapPointMain.hs"
              , source-dirs =
                  "prod-apps/gap-point"
              }
          , test-fsdb-parser =
              { dependencies =
                  depsTest # [ "uom-plugin", "flight-fsdb", "flight-units" ]
              , ghc-options =
                  [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
              , main =
                  "FsdbMain.hs"
              , source-dirs =
                  "test-apps/fsdb-parser"
              }
          , test-igc-parser =
              { dependencies =
                  depsTest # [ "flight-igc" ]
              , ghc-options =
                  [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
              , main =
                  "IgcMain.hs"
              , source-dirs =
                  "test-apps/igc-parser"
              }
          , test-kml-parser =
              { dependencies =
                  depsTest # [ "flight-kml" ]
              , ghc-options =
                  [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
              , main =
                  "KmlMain.hs"
              , source-dirs =
                  "test-apps/kml-parser"
              }
          }
      , tests =
          ./../default-tests.dhall
      }
