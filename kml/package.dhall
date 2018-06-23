  ./defaults.dhall 
⫽ { name =
      "flight-kml"
  , synopsis =
      "A parser for KML files."
  , description =
      "KML can be a source of flight waypoints."
  , category =
      "Data, Parsing"
  , github =
      "blockscope/flare-timing/kml"
  , library =
      { dependencies =
          [ "base >=4.5 && <5"
          , "split"
          , "parsec"
          , "hxt"
          , "path"
          , "hxt-xpath"
          , "aeson"
          , "time"
          , "detour-via-sci"
          , "siggy-chardust"
          ]
      , source-dirs =
          "library"
      , exposed-modules =
          "Flight.Kml"
      }
  , tests =
      { doctest =
          { dependencies =
              [ "base"
              , "split"
              , "parsec"
              , "hxt"
              , "path"
              , "hxt-xpath"
              , "aeson"
              , "time"
              , "hlint"
              , "detour-via-sci"
              , "siggy-chardust"
              , "doctest"
              ]
          , ghc-options =
              [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
          , main =
              "DocTest.hs"
          , source-dirs =
              [ "library", "test-suite-doctest" ]
          }
      , hlint =
          { dependencies =
              [ "base"
              , "split"
              , "parsec"
              , "hxt"
              , "path"
              , "hxt-xpath"
              , "aeson"
              , "time"
              , "hlint"
              , "detour-via-sci"
              , "siggy-chardust"
              ]
          , ghc-options =
              [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
          , main =
              "HLint.hs"
          , source-dirs =
              [ "library", "test-suite-hlint" ]
          }
      , parse =
          { dependencies =
              [ "base"
              , "split"
              , "parsec"
              , "hxt"
              , "path"
              , "hxt-xpath"
              , "aeson"
              , "time"
              , "raw-strings-qq"
              , "tasty"
              , "tasty-hunit"
              , "tasty-quickcheck"
              , "tasty-smallcheck"
              , "smallcheck"
              , "detour-via-sci"
              , "siggy-chardust"
              ]
          , ghc-options =
              [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
          , main =
              "Parse.hs"
          , source-dirs =
              [ "library", "test-suite-parse" ]
          }
      }
  }
