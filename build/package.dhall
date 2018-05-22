  ./defaults.dhall 
⫽ { name =
      "build-flare-timing"
  , synopsis =
      "A shake build of flare-timing."
  , description =
      "Builds the packages making up flare-timing."
  , category =
      "Data, Parsing"
  , github =
      "blockscope/flare-timing/build"
  , executables =
      { build-flare-timing =
          { dependencies =
              [ "base", "ansi-terminal", "shake", "time" ]
          , ghc-options =
              [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
          , main =
              "Main.hs"
          , source-dirs =
              "app-cmd"
          }
      }
  , tests =
      { hlint =
          { dependencies =
              [ "base", "hlint" ]
          , ghc-options =
              [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
          , main =
              "HLint.hs"
          , source-dirs =
              [ "test-suite-hlint" ]
          }
      }
  }
