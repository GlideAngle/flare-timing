./defaults.dhall //
{ name =
    "www-flare-timing"
, synopsis =
    "A collection of apps and libraries for scoring hang gliding and paragliding competitions."
, description =
    "Scoring and viewing hang gliding and paragliding competitions."
, category =
    "Data, Parsing"
, github =
    "blockscope/flare-timing/www"
, executables =
    { comp-serve =
        { dependencies =
            [ "base"
            , "servant"
            , "servant-server"
            , "transformers"
            , "aeson"
            , "wai"
            , "wai-cors"
            , "warp"
            , "directory"
            , "filepath"
            , "system-filepath"
            , "filemanip"
            , "raw-strings-qq"
            , "cmdargs"
            , "mtl"
            , "transformers"
            , "yaml"
            , "bytestring"
            , "flight-comp"
            ]
        , ghc-options =
            [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
        , main =
            "Main.hs"
        , source-dirs =
            "serve"
        }
    }
, tests =
    { hlint =
        { dependencies =
            [ "base", "hlint", "flight-comp" ]
        , ghc-options =
            [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
        , main =
            "HLint.hs"
        , source-dirs =
            "test-suite-hlint"
        }
    }
}
