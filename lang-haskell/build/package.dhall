let mkHome = ./../home.dhall

in    ./../defaults.dhall
    ⫽ { name = "ft-build"
      , homepage = mkHome "lang-haskell/build#readme"
      , synopsis = "A shake build of flare-timing."
      , description = "Builds the packages making up flare-timing."
      , category = "Data, Parsing"
      , executables.ft-build
        =
        { dependencies =
          [ "base"
          , "ansi-terminal"
          , "dhall"
          , "shake"
          , "raw-strings-qq"
          , "text"
          , "time"
          ]
        , ghc-options = [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
        , main = "Main.hs"
        , source-dirs = [ "app-cmd", "library" ]
        }
      , tests = ./../default-tests.dhall
      }
