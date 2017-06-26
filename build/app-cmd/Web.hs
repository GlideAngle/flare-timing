module Web (buildRules, cleanRules) where

import Development.Shake
    ( Rules
    , CmdOption(Cwd)
    , removeFilesAfter
    , phony
    , cmd
    , need
    )
import Development.Shake.FilePath ((</>))

cleanRules :: Rules ()
cleanRules = do
    phony "clean-view-reflex" $ do
        removeFilesAfter
            ("view" </> "task.jsexe")
            [ "all.js"
            , "all.js.externs"
            , "index.html"
            , "lib.js"
            , "manifest.webapp"
            , "out.js"
            , "out.stats"
            , "rts.js"
            , "runmain.js"
            ]

buildRules :: Rules ()
buildRules = do
    phony "view-reflex" $ do
        need [ "view" </> "task.jsexe/index.html" ]
        cmd (Cwd "view") "ghcjs task.hs"
