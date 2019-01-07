    let defs = ./../defaults.dhall

in    defs
    ⫽ ./../default-extensions.dhall
    ⫽ { name =
          "flight-scribe"
      , synopsis =
          "Hang gliding and paragliding competition scoring files."
      , description =
          "Reading and writing competition scoring files."
      , category =
          "Data"
      , github =
          "blockscope/flare-timing/scribe"
      , ghc-options =
          [ "-Wall", "-Wwarn" ]
      , dependencies =
            defs.dependencies
          # [ "split"
            , "path"
            , "scientific"
            , "containers"
            , "unordered-containers"
            , "time"
            , "cassava"
            , "directory"
            , "filemanip"
            , "mtl"
            , "safe-exceptions"
            , "vector"
            , "megaparsec >= 7.0.1"
            , "dhall >= 1.18.0"
            , "dhall-json >= 1.2.4"
            , "transformers"
            , "text"
            , "microlens"
            , "filepath"
            , "bytestring"
            , "prettyprinter"
            , "aeson"
            , "aeson-pretty"
            , "yaml"
            , "detour-via-sci"
            , "uom-plugin"
            , "contravariant"
            , "flight-latlng"
            , "flight-zone"
            , "flight-route"
            , "flight-gap"
            , "flight-comp"
            ]
      , library =
          { source-dirs = "library", exposed-modules = "Flight.Scribe" }
      , tests =
          ./../default-tests.dhall
      }
