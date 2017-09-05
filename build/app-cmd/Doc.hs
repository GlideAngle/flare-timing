module Doc (buildRules, cleanRules) where

import Development.Shake
    ( Rules
    , CmdOption(Shell, Cwd)
    , removeFilesAfter
    , phony
    , cmd
    , need
    )
import Development.Shake.FilePath (FilePath, (</>))

docFor :: String -> String
docFor x =
    "stack exec -- haddock --html --hyperlinked-source --odir=../__docs/" ++ x

cleanRules :: Rules ()
cleanRules =
    phony "clean-docs" $
        removeFilesAfter "__docs" [ "//*" ] 

root :: FilePath
root = "flare-timing"

-- NOTE: Stack doesn't build docs for executables.
-- SEE: https://github.com/commercialhaskell/stack/issues/729
-- SEE: https://www.reddit.com/r/haskell/comments/5ugm9s/how_to_generate_haddock_docs_for_nonlibrary_code/
buildRules :: Rules ()
buildRules = do
    phony "docs" $
        need [ "mask-docs"
             , "app-serve-docs"
             , "comp-xml-to-yaml-docs"
             , "fsdb-docs"
             , "igc-docs"
             , "kml-docs"
             ]

    phony "mask-docs" $
        cmd
            (Cwd root)
            Shell
            (docFor "mask")
            [ "mask/app-cmd/Cmd/Options.hs" ]

    phony "app-serve-docs" $
        cmd
            (Cwd root)
            Shell
            (docFor "app-serve")
            [ "yaml/app-serve/Serve/Options.hs" ]

    phony "comp-xml-to-yaml-docs" $
        cmd
            (Cwd root)
            Shell
            (docFor "comp-xml-to-yaml")
            [ "yaml/comp-xml-to-yaml/Cmd/Options.hs" ]

    phony "fsdb-docs" $
        cmd
            (Cwd root)
            Shell
            (docFor "fsdb-cmd")
            [ "fsdb/app-cmd/Cmd/Options.hs" ]

    phony "igc-docs" $
        cmd
            (Cwd root)
            Shell
            (docFor "igc-cmd")
            [ "igc/app-cmd/Igc/Options.hs" ]

    phony "kml-docs" $
        cmd
            (Cwd root)
            Shell
            (docFor "kml-cmd")
            [ "kml/app-cmd/Kml/Options.hs" ]
