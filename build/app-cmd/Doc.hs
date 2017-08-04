module Doc (buildRules, cleanRules) where

import Development.Shake
    ( Rules
    , CmdOption(Shell)
    , removeFilesAfter
    , phony
    , cmd
    , need
    )
import Development.Shake.FilePath ((</>))

cleanRules :: Rules ()
cleanRules = do
    phony "clean-docs" $
        removeFilesAfter "__docs" [ "//*" ] 

-- NOTE: Stack doesn't build docs for exucutables.
-- SEE: https://github.com/commercialhaskell/stack/issues/729
-- SEE: https://www.reddit.com/r/haskell/comments/5ugm9s/how_to_generate_haddock_docs_for_nonlibrary_code/
buildRules :: Rules ()
buildRules = do
    phony "docs" $
        need [ "track-docs"
             ]

    phony "track-docs" $ do
        let files =
                ("Track/app-cmd/Cmd" </>) <$>
                [ "Options.hs"
                ]
        cmd
            Shell
            [ "stack exec -- haddock --html --hyperlinked-source --odir=__docs/track"
            ]
            files
