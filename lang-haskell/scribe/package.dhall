    let mkHome = ./../home.dhall

in  let defs = ./../defaults.dhall

in    defs
    ⫽ ./../default-extensions.dhall
    ⫽ { name =
          "flight-scribe"
      , homepage =
          mkHome "lang-haskell/scribe#readme"
      , synopsis =
          "Hang gliding and paragliding competition scoring files."
      , description =
          "Reading and writing competition scoring files."
      , category =
          "Data"
      , ghc-options =
          [ "-Wall"
          , "-fplugin Data.UnitsOfMeasure.Plugin"
          , "-fno-warn-partial-type-signatures"
          ]
      , dependencies =
            defs.dependencies
          # [ "split"
            , "path"
            , "aeson"
            , "deepseq"
            , "scientific"
            , "containers"
            , "unordered-containers"
            , "time"
            , "cassava"
            , "bytestring"
            , "directory"
            , "filepath"
            , "filemanip"
            , "mtl"
            , "parallel-io"
            , "safe-exceptions"
            , "text"
            , "yaml"
            , "vector"
            , "uom-plugin"
            , "detour-via-sci"
            , "flight-clip"
            , "flight-comp"
            , "flight-gap-allot"
            , "flight-gap-lead"
            , "flight-latlng"
            , "flight-route"
            , "flight-units"
            , "flight-zone"
            ]
      , library =
          { source-dirs = "library", exposed-modules = "Flight.Scribe" }
      , tests =
          ./../default-tests.dhall
      }
