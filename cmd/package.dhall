  ./defaults.dhall 
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
  , library =
      { dependencies =
          [ "base >=4.5 && <5"
          , "directory"
          , "filepath"
          , "system-filepath"
          , "filemanip"
          , "raw-strings-qq"
          , "cmdargs"
          , "mtl"
          , "transformers"
          , "flight-span"
          ]
      , source-dirs =
          "library"
      , exposed-modules =
          [ "Flight.Cmd.Options", "Flight.Cmd.Paths" ]
      }
  , tests =
      { hlint =
          { dependencies =
              [ "base"
              , "hlint"
              , "directory"
              , "filepath"
              , "system-filepath"
              , "filemanip"
              , "raw-strings-qq"
              , "cmdargs"
              , "mtl"
              , "transformers"
              , "flight-span"
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
