module Doc (buildRules, cleanRules) where

import Development.Shake
    ( Rules
    , CmdOption(Shell)
    , removeFilesAfter
    , phony
    , cmd
    , need
    )

docFor :: String -> String
docFor x =
    "stack exec -- haddock --html --hyperlinked-source --odir=__docs/" ++ x

cleanRules :: Rules ()
cleanRules =
    phony "clean-docs" $
        removeFilesAfter "__docs" [ "//*" ] 

-- NOTE: Stack doesn't build docs for exucutables.
-- SEE: https://github.com/commercialhaskell/stack/issues/729
-- SEE: https://www.reddit.com/r/haskell/comments/5ugm9s/how_to_generate_haddock_docs_for_nonlibrary_code/
buildRules :: Rules ()
buildRules = do
    phony "docs" $
        need [ "track-docs"
             , "fsdb-docs"
             , "igc-docs"
             , "kml-docs"
             ]

    phony "track-docs" $
        cmd
            Shell
            (docFor "track-cmd")
            [ "track/app-cmd/Cmd/Options.hs" ]

    phony "fsdb-docs" $
        cmd
            Shell
            (docFor "fsdb-cmd")
            [ "fsdb/app-cmd/Cmd/Options.hs" ]

    phony "igc-docs" $
        cmd
            Shell
            (docFor "igc-cmd")
            [ "igc/app-cmd/Igc/Options.hs" ]

    phony "kml-docs" $
        cmd
            Shell
            (docFor "kml-cmd")
            [ "kml/app-cmd/Kml/Options.hs" ]
