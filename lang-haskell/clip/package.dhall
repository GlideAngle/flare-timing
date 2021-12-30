let mkHome = ./../home.dhall

in  let defs = ./../defaults.dhall

    in    defs
        ⫽ ./../default-extensions.dhall
        ⫽ { name = "flight-clip"
          , homepage = mkHome "lang-haskell/clip#readme"
          , version = "1.1.0"
          , synopsis = "Clipping a pilot's tracklogs."
          , description =
              "Provides types and typeclasses for clipping a tracklog."
          , category = "Data, Parsing, Flight"
          , dependencies = defs.dependencies # [ "split", "time" ]
          , library =
            { source-dirs = "library"
            , exposed-modules = [ "Flight.Clip", "Flight.Track.Range" ]
            }
          , tests =
                ./../default-tests.dhall
              ⫽ { doctest =
                  { dependencies = [ "doctest", "QuickCheck" ]
                  , ghc-options =
                    [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
                  , main = "DocTest.hs"
                  , source-dirs = [ "library", "test-suite-doctest" ]
                  , when =
                    { condition = "flag(suppress-failing-tests)"
                    , buildable = False
                    }
                  }
                }
          }
