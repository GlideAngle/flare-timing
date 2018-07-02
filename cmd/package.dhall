    let defs = ./defaults.dhall 

in    defs
    ⫽ { name =
          "flight-cmd"
      , synopsis =
          "Command line options."
      , description =
          "Command line options such as file or directory, task or pilot."
      , category =
          "Flight"
      , github =
          "blockscope/flare-timing/cmd"
      , dependencies =
            defs.dependencies
          # [ "directory"
            , "filepath"
            , "system-filepath"
            , "filemanip"
            , "raw-strings-qq"
            , "cmdargs"
            , "mtl"
            , "transformers"
            , "flight-span"
            ]
      , library =
          { source-dirs =
              "library"
          , exposed-modules =
              [ "Flight.Cmd.Options", "Flight.Cmd.Paths" ]
          }
      , tests =
          { hlint =
              { dependencies =
                  [ "hlint" ]
              , ghc-options =
                  [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
              , main =
                  "HLint.hs"
              , source-dirs =
                  [ "library", "test-suite-hlint" ]
              }
          }
      }
