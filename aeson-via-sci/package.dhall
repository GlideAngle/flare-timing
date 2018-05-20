{ name =
    "aeson-via-sci"
, version =
    "0.1.0"
, synopsis =
    "JSON encoding and decoding for rationals via scientific."
, description =
    "Lossy JSON encoding for rationals."
, category =
    "Flight"
, github =
    "blockscope/flare-timing/aeson-via-sci"
, author =
    "Phil de Joux"
, maintainer =
    "phil.dejoux@blockscope.com"
, copyright =
    "Phil de Joux 2017-2018"
, license =
    "MPL-2.0"
, license-file =
    "LICENSE.md"
, tested-with =
    "GHC == 8.2.2"
, extra-source-files =
    [ "package.dhall", "changelog.md", "README.md" ]
, ghc-options =
    [ "-Wall" ]
, library =
    { dependencies =
        [ "base >=4.5 && <5"
        , "newtype"
        , "scientific"
        , "aeson"
        , "cassava"
        , "template-haskell"
        ]
    , source-dirs =
        "library"
    , exposed-modules =
        [ "Data.Aeson.Via.Scientific" ]
    }
, tests =
    { hlint =
        { dependencies =
            [ "base"
            , "hlint"
            , "newtype"
            , "scientific"
            , "aeson"
            , "cassava"
            , "template-haskell"
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
