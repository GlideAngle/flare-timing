-- {=}
-- NOTE: To switch hlint tests alternate commenting lines above or below.
{ hlint =
    { dependencies =
        [ "hlint" ]
    , ghc-options =
        [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
    , main =
        "HLint.hs"
    , source-dirs =
        [ "library", "test-suite-hlint" ]
    }
}
