module Web (buildRules, cleanRules) where

import Development.Shake
    ( Rules
    , CmdOption(Cwd, Shell)
    , (%>)
    , removeFilesAfter
    , phony
    , cmd
    , need
    , copyFile'
    )
import Development.Shake.FilePath ((</>), (<.>))

ghcjsOutputs :: [ String ]
ghcjsOutputs =
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

cleanRules :: Rules ()
cleanRules = do
    phony "clean-view-reflex" $ do
        removeFilesAfter ("view" </> "task.jsexe") ghcjsOutputs

buildRules :: Rules ()
buildRules = do
    phony "view-www" $ do
        need [ "__www" </> "task-view" </> "all" <.> "js"
             , "__www" </> "task-view" </> "task" <.> "html"
             ]

    phony "view-reflex" $ do
        need [ "view" </> "task.jsexe" </> "all" <.> "js" ]

    "view" </> "task.jsexe" </> "all" <.> "js" %> \ _ -> do
        -- TODO: Install ghcjs on windows.
        cmd (Cwd "view") "echo ghcjs -DGHCJS_BROWSER task.hs"

    "__www" </> "task-view" </> "task" <.> "html" %> \ _ -> do
        cmd (Cwd "view") Shell "yarn run pack"

    mconcat $ (\ s ->
        "__www" </> "task-view" </> s %> \ _ -> do
            copyFile'
                ("view" </> "task.jsexe" </> s)
                ("__www" </> "task-view" </> s))
        <$> ghcjsOutputs
