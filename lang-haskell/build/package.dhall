let mkHome = ./../home.dhall

in    ./../defaults.dhall
    ⫽ { name = "ft-build"
      , homepage = mkHome "lang-haskell/build#readme"
      , synopsis = "A shake build of flare-timing."
      , description = "Builds the packages making up flare-timing."
      , category = "Data, Parsing"
      , dependencies =
        [ "base"
        , "shake"
        , "ansi-terminal"
        , "dhall"
        , "raw-strings-qq"
        , "text"
        , "time"
        ]
      , executables.ft-build
        =
        { ghc-options = [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
        , main = "Main.hs"
        , source-dirs = [ "app-cmd", "library" ]
        }
      , tests = ./../default-tests.dhall
      }
