  ./../defaults.dhall
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
              [ "base"
              , "ansi-terminal"
              , "dhall"
              , "shake"
              , "raw-strings-qq"
              , "text"
              , "time"
              ]
          , ghc-options =
              [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
          , main =
              "Main.hs"
          , source-dirs =
              [ "app-cmd", "library" ]
          }
      }
  , tests =
      ./../default-tests.dhall
  }
