    let defs = ./../defaults.dhall

in  let depsTest =
            defs.dependencies
          # [ "cmdargs"
            , "directory"
            , "filemanip"
            , "filepath"
            , "raw-strings-qq"
            , "flight-cmd"
            , "flight-comp"
            ]

in  let deps =
            defs.dependencies
          # [ "aeson"
            , "bytestring"
            , "clock"
            , "cmdargs"
            , "directory"
            , "filemanip"
            , "filepath"
            , "formatting"
            , "mtl"
            , "raw-strings-qq"
            , "transformers"
            , "uom-plugin"
            , "yaml"
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
      , homepage =
          "https://github.com/BlockScope/flare-timing#readme"
      , synopsis =
          "A collection of apps and libraries for scoring hang gliding and paragliding competitions."
      , description =
          "Scoring and viewing hang gliding and paragliding competitions."
      , category =
          "Data, Parsing"
      , flags =
          { suppress-failing-tests =
              { manual = False, default = True }
          , suppress-test-parsers =
              { manual = False, default = True }
          }
      , executables =
          { ft-extract-input =
              { dependencies =
                    deps
                  # [ "parallel-io"
                    , "flight-earth"
                    , "flight-fsdb"
                    , "flight-gap-allot"
                    , "flight-gap-math"
                    , "flight-gap-stop"
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
          , fs-arrival =
              { dependencies =
                    deps
                  # [ "containers"
                    , "flight-earth"
                    , "flight-fsdb"
                    , "flight-gap-allot"
                    , "flight-gap-stop"
                    , "flight-mask"
                    , "flight-route"
                    , "flight-zone"
                    ]
              , ghc-options =
                  [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
              , main =
                  "FsArrivalMain.hs"
              , source-dirs =
                  "prod-apps/fs-arrival"
              }
          , fs-effort =
              { dependencies =
                    deps
                  # [ "containers"
                    , "flight-earth"
                    , "flight-fsdb"
                    , "flight-gap-allot"
                    , "flight-mask"
                    , "flight-route"
                    , "flight-zone"
                    ]
              , ghc-options =
                  [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
              , main =
                  "FsEffortMain.hs"
              , source-dirs =
                  "prod-apps/fs-effort"
              }
          , fs-clean =
              { dependencies =
                    deps
                  # [ "containers"
                    , "flight-earth"
                    , "flight-fsdb"
                    , "flight-gap-allot"
                    , "flight-mask"
                    , "flight-route"
                    , "flight-zone"
                    ]
              , ghc-options =
                  [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
              , main =
                  "FsCleanMain.hs"
              , source-dirs =
                  "prod-apps/fs-clean"
              }
          , fs-trim =
              { dependencies =
                    deps
                  # [ "containers"
                    , "flight-earth"
                    , "flight-fsdb"
                    , "flight-gap-allot"
                    , "flight-mask"
                    , "flight-route"
                    , "flight-zone"
                    ]
              , ghc-options =
                  [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
              , main =
                  "FsTrimMain.hs"
              , source-dirs =
                  "prod-apps/fs-trim"
              }
          , fs-route =
              { dependencies =
                    deps
                  # [ "containers"
                    , "flight-earth"
                    , "flight-fsdb"
                    , "flight-gap-allot"
                    , "flight-mask"
                    , "flight-route"
                    , "flight-zone"
                    ]
              , ghc-options =
                  [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
              , main =
                  "FsRouteMain.hs"
              , source-dirs =
                  "prod-apps/fs-route"
              }
          , fs-score =
              { dependencies =
                    deps
                  # [ "containers"
                    , "flight-earth"
                    , "flight-fsdb"
                    , "flight-gap-allot"
                    , "flight-gap-math"
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
          , ft-task-length =
              { dependencies =
                    deps
                  # [ "safe-exceptions"
                    , "flight-earth"
                    , "flight-route"
                    , "flight-units"
                    , "flight-zone"
                    ]
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
          , ft-fly-time =
              { dependencies =
                    deps
                  # [ "lens"
                    , "deepseq"
                    , "parallel-io"
                    , "safe-exceptions"
                    , "siggy-chardust"
                    , "flight-earth"
                    , "flight-mask"
                    , "flight-span"
                    , "flight-track"
                    , "flight-units"
                    , "flight-zone"
                    ]
              , ghc-options =
                  [ "-rtsopts"
                  , "-threaded"
                  , "-with-rtsopts=-N"
                  , "-Wall"
                  , "-fplugin Data.UnitsOfMeasure.Plugin"
                  ]
              , main =
                  "FlyTimeMain.hs"
              , source-dirs =
                  "prod-apps/fly-time"
              }
          , ft-cross-zone =
              { dependencies =
                    deps
                  # [ "lens"
                    , "deepseq"
                    , "parallel-io"
                    , "safe-exceptions"
                    , "siggy-chardust"
                    , "flight-earth"
                    , "flight-mask"
                    , "flight-span"
                    , "flight-track"
                    , "flight-units"
                    , "flight-zone"
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
          , ft-tag-zone =
              { dependencies =
                    deps
                  # [ "safe-exceptions"
                    , "time"
                    , "flight-earth"
                    , "flight-mask"
                    , "flight-span"
                    , "flight-zone"
                    ]
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
          , ft-unpack-track =
              { dependencies =
                    deps
                  # [ "lens"
                    , "deepseq"
                    , "parallel-io"
                    , "safe-exceptions"
                    , "siggy-chardust"
                    , "time"
                    , "flight-kml"
                    , "flight-lookup"
                    , "flight-mask"
                    , "flight-track"
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
          , ft-peg-frame =
              { dependencies =
                    deps
                  # [ "containers"
                    , "safe-exceptions"
                    , "time"
                    , "flight-clip"
                    , "flight-zone"
                    ]
              , ghc-options =
                  [ "-rtsopts"
                  , "-threaded"
                  , "-with-rtsopts=-N"
                  , "-Wall"
                  , "-fplugin Data.UnitsOfMeasure.Plugin"
                  ]
              , main =
                  "PegFrameMain.hs"
              , source-dirs =
                  "prod-apps/peg-frame"
              }
          , ft-align-time =
              { dependencies =
                    deps
                  # [ "lens"
                    , "siggy-chardust"
                    , "safe-exceptions"
                    , "flight-kml"
                    , "flight-lookup"
                    , "flight-mask"
                    , "flight-zone"
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
          , ft-discard-further =
              { dependencies =
                    deps
                  # [ "safe-exceptions"
                    , "vector"
                    , "flight-clip"
                    , "flight-gap-allot"
                    , "flight-lookup"
                    , "flight-mask"
                    , "flight-route"
                    , "flight-zone"
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
          , ft-lead-area =
              { dependencies =
                    deps
                  # [ "safe-exceptions"
                    , "vector"
                    , "flight-clip"
                    , "flight-gap-allot"
                    , "flight-gap-lead"
                    , "flight-lookup"
                    , "flight-mask"
                    , "flight-route"
                    , "flight-zone"
                    ]
              , ghc-options =
                  [ "-rtsopts"
                  , "-threaded"
                  , "-with-rtsopts=-N"
                  , "-Wall"
                  , "-fplugin Data.UnitsOfMeasure.Plugin"
                  ]
              , main =
                  "LeadAreaMain.hs"
              , source-dirs =
                  "prod-apps/lead-area"
              }
          , ft-mask-arrival =
              { dependencies =
                    deps
                  # [ "containers"
                    , "deepseq"
                    , "parallel-io"
                    , "lens"
                    , "siggy-chardust"
                    , "safe-exceptions"
                    , "statistics"
                    , "time"
                    , "vector"
                    , "flight-clip"
                    , "flight-earth"
                    , "flight-gap-allot"
                    , "flight-gap-valid"
                    , "flight-kml"
                    , "flight-lookup"
                    , "flight-mask"
                    , "flight-route"
                    , "flight-span"
                    , "flight-track"
                    , "flight-zone"
                    ]
              , ghc-options =
                  [ "-rtsopts"
                  , "-threaded"
                  , "-with-rtsopts=-N"
                  , "-Wall"
                  , "-fplugin Data.UnitsOfMeasure.Plugin"
                  ]
              , other-modules =
                  [ "Mask"
                  , "MaskArrival"
                  , "MaskArrivalOptions"
                  , "MaskPilots"
                  , "MaskSpeed"
                  , "Stats"
                  , "Paths_flare_timing"
                  ]
              , main =
                  "MaskArrivalMain.hs"
              , source-dirs =
                  [ "prod-apps/mask-arrival" ]
              }
          , ft-mask-reach =
              { dependencies =
                    deps
                  # [ "containers"
                    , "deepseq"
                    , "parallel-io"
                    , "lens"
                    , "siggy-chardust"
                    , "safe-exceptions"
                    , "statistics"
                    , "time"
                    , "vector"
                    , "flight-clip"
                    , "flight-earth"
                    , "flight-gap-allot"
                    , "flight-gap-lead"
                    , "flight-gap-valid"
                    , "flight-kml"
                    , "flight-lookup"
                    , "flight-mask"
                    , "flight-route"
                    , "flight-span"
                    , "flight-track"
                    , "flight-zone"
                    ]
              , ghc-options =
                  [ "-rtsopts"
                  , "-threaded"
                  , "-with-rtsopts=-N"
                  , "-Wall"
                  , "-fplugin Data.UnitsOfMeasure.Plugin"
                  ]
              , other-modules =
                  [ "Mask.Reach.Tick"
                  , "Mask.Reach.Time"
                  , "MaskEffort"
                  , "MaskLead"
                  , "MaskLeadCoef"
                  , "MaskPilots"
                  , "MaskSpeed"
                  , "Mask"
                  , "MaskReachOptions"
                  , "Stats"
                  , "Paths_flare_timing"
                  ]
              , main =
                  "MaskReachMain.hs"
              , source-dirs =
                  [ "prod-apps/mask-common", "prod-apps/mask-reach" ]
              }
          , ft-mask-bonus =
              { dependencies =
                    deps
                  # [ "containers"
                    , "deepseq"
                    , "parallel-io"
                    , "lens"
                    , "siggy-chardust"
                    , "safe-exceptions"
                    , "statistics"
                    , "time"
                    , "vector"
                    , "flight-clip"
                    , "flight-earth"
                    , "flight-gap-allot"
                    , "flight-gap-lead"
                    , "flight-gap-valid"
                    , "flight-kml"
                    , "flight-lookup"
                    , "flight-mask"
                    , "flight-route"
                    , "flight-span"
                    , "flight-track"
                    , "flight-zone"
                    ]
              , ghc-options =
                  [ "-rtsopts"
                  , "-threaded"
                  , "-with-rtsopts=-N"
                  , "-Wall"
                  , "-fplugin Data.UnitsOfMeasure.Plugin"
                  ]
              , other-modules =
                  [ "MaskEffort"
                  , "MaskLead"
                  , "MaskLeadCoef"
                  , "MaskPilots"
                  , "MaskSpeed"
                  , "Mask"
                  , "Mask.Reach.Tick"
                  , "MaskBonusOptions"
                  , "Stats"
                  , "Paths_flare_timing"
                  ]
              , main =
                  "MaskBonusMain.hs"
              , source-dirs =
                  [ "prod-apps/mask-common", "prod-apps/mask-bonus" ]
              }
          , ft-mask-effort =
              { dependencies =
                    deps
                  # [ "containers"
                    , "deepseq"
                    , "parallel-io"
                    , "lens"
                    , "siggy-chardust"
                    , "safe-exceptions"
                    , "statistics"
                    , "time"
                    , "vector"
                    , "flight-clip"
                    , "flight-earth"
                    , "flight-gap-allot"
                    , "flight-gap-lead"
                    , "flight-gap-valid"
                    , "flight-kml"
                    , "flight-lookup"
                    , "flight-mask"
                    , "flight-route"
                    , "flight-span"
                    , "flight-track"
                    , "flight-zone"
                    ]
              , ghc-options =
                  [ "-rtsopts"
                  , "-threaded"
                  , "-with-rtsopts=-N"
                  , "-Wall"
                  , "-fplugin Data.UnitsOfMeasure.Plugin"
                  ]
              , other-modules =
                  [ "MaskEffort"
                  , "MaskLead"
                  , "MaskLeadCoef"
                  , "MaskPilots"
                  , "MaskSpeed"
                  , "Mask"
                  , "MaskEffortOptions"
                  , "Stats"
                  , "Paths_flare_timing"
                  ]
              , main =
                  "MaskEffortMain.hs"
              , source-dirs =
                  [ "prod-apps/mask-common", "prod-apps/mask-effort" ]
              }
          , ft-mask-lead =
              { dependencies =
                    deps
                  # [ "containers"
                    , "deepseq"
                    , "parallel-io"
                    , "lens"
                    , "siggy-chardust"
                    , "safe-exceptions"
                    , "statistics"
                    , "time"
                    , "vector"
                    , "flight-clip"
                    , "flight-earth"
                    , "flight-gap-allot"
                    , "flight-gap-lead"
                    , "flight-gap-valid"
                    , "flight-kml"
                    , "flight-lookup"
                    , "flight-mask"
                    , "flight-route"
                    , "flight-span"
                    , "flight-track"
                    , "flight-zone"
                    ]
              , ghc-options =
                  [ "-rtsopts"
                  , "-threaded"
                  , "-with-rtsopts=-N"
                  , "-Wall"
                  , "-fplugin Data.UnitsOfMeasure.Plugin"
                  ]
              , other-modules =
                  [ "MaskEffort"
                  , "MaskLead"
                  , "MaskLeadCoef"
                  , "MaskPilots"
                  , "MaskSpeed"
                  , "Mask"
                  , "MaskLeadOptions"
                  , "Stats"
                  , "Paths_flare_timing"
                  ]
              , main =
                  "MaskLeadMain.hs"
              , source-dirs =
                  [ "prod-apps/mask-common", "prod-apps/mask-lead" ]
              }
          , ft-land-out =
              { dependencies =
                    deps
                  # [ "safe-exceptions"
                    , "flight-gap-allot"
                    , "flight-gap-effort"
                    ]
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
          , ft-far-out =
              { dependencies =
                    deps
                  # [ "safe-exceptions"
                    , "time"
                    , "flight-lookup"
                    , "flight-route"
                    , "flight-gap-allot"
                    , "flight-gap-effort"
                    , "flight-gap-valid"
                    ]
              , other-modules =
                  [ "MaskPilots", "Stats", "FarOutOptions" ]
              , ghc-options =
                  [ "-rtsopts"
                  , "-threaded"
                  , "-with-rtsopts=-N"
                  , "-Wall"
                  , "-fplugin Data.UnitsOfMeasure.Plugin"
                  ]
              , main =
                  "FarOutMain.hs"
              , source-dirs =
                  [ "prod-apps/mask-common"
                  , "prod-apps/mask-effort"
                  , "prod-apps/far-out"
                  ]
              }
          , ft-gap-point =
              { dependencies =
                    deps
                  # [ "containers"
                    , "facts"
                    , "newtype"
                    , "safe-exceptions"
                    , "time"
                    , "flight-gap-allot"
                    , "flight-gap-effort"
                    , "flight-gap-lead"
                    , "flight-gap-math"
                    , "flight-gap-valid"
                    , "flight-gap-weight"
                    , "flight-lookup"
                    , "flight-mask"
                    , "flight-route"
                    , "flight-zone"
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
          , ft-fsdb-parser =
              { dependencies =
                  depsTest # [ "uom-plugin", "flight-fsdb", "flight-units" ]
              , ghc-options =
                  [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
              , main =
                  "FsdbMain.hs"
              , source-dirs =
                  "test-apps/fsdb-parser"
              , when =
                  { condition =
                      "flag(suppress-test-parsers)"
                  , buildable =
                      False
                  }
              }
          , ft-igc-parser =
              { dependencies =
                  depsTest # [ "flight-igc" ]
              , ghc-options =
                  [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
              , main =
                  "IgcMain.hs"
              , source-dirs =
                  "test-apps/igc-parser"
              , when =
                  { condition =
                      "flag(suppress-test-parsers)"
                  , buildable =
                      False
                  }
              }
          , ft-kml-parser =
              { dependencies =
                  depsTest # [ "flight-kml" ]
              , ghc-options =
                  [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
              , main =
                  "KmlMain.hs"
              , source-dirs =
                  "test-apps/kml-parser"
              , when =
                  { condition =
                      "flag(suppress-test-parsers)"
                  , buildable =
                      False
                  }
              }
          }
      , tests =
          ./../default-tests.dhall
      }
