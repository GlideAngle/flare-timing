  ./defaults.dhall 
⫽ { name =
      "flight-kml"
  , version =
      "1.0.0"
  , synopsis =
      "Parsing of pilot tracklogs dumped as KML."
  , description =
      "Provides parsing of dumped tracklogs. In hang gliding and paragliding competitions when FS and GpsDump are paired in competition mode a pilot's tracklog is dumped as KML. This is exlained in detail on the FS wiki."
  , category =
      "Data, Parsing"
  , github =
      "blockscope/flare-timing/kml"
  , homepage =
      "https://github.com/blockscope/flare-timing/tree/master/kml#readme"
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
          [ "Flight.Kml", "Flight.Kml.Internal" ]
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
              , "template-haskell"
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
