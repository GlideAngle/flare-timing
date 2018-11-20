{-# LANGUAGE QuasiQuotes #-}

module Web (buildRules, cleanRules) where

import Text.RawString.QQ
import Development.Shake
    ( Rules
    , CmdOption(Cwd, Shell)
    , (%>)
    , removeFilesAfter
    , phony
    , cmd
    , need
    , copyFileChanged
    , putNormal
    )
import Development.Shake.FilePath (FilePath, (</>))

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
cleanRules =
    phony "clean-www" $ do
        removeFilesAfter out [ "//*" ]
        removeFilesAfter tmp [ "//*" ]

out :: FilePath
out = "__www-dist"

tmp :: FilePath
tmp = "__www-build"

view :: FilePath
view = "app-view" </> "comp-view"

buildRules :: Rules ()
buildRules = do
    phony "view-start" $
        cmd (Cwd view) Shell "yarn run start"

    phony "view-www" $ do
        need $ (\ s -> out </> "task-view" </> s)
             <$> [ "all.js", "app.html" ]

    phony "view-reflex" $ do
        need [ out </> "task.jsexe" </> "all.js" ]

    view </> "node_modules" </> "webpack" </> "package.json" %> \ _ -> do
        putNormal "# install node_modules" 
        cmd (Cwd view) Shell "yarn install"

    out </> "task-view" </> "app.html" %> \ _ -> do
        putNormal "# pack app.html" 
        need [ view </> "node_modules" </> "webpack" </> "package.json" ]
        cmd (Cwd view) Shell "yarn run pack"

    mconcat $ (\ s ->
        tmp </> "app.jsexe" </> s %> \ _ -> do
            need [ view </> "App.hs"
                 , view </> "FlareTiming" </> "Task.hs"
                 , view </> "FlareTiming" </> "Map.hs"
                 ]

            putNormal ghcjsCmd
            cmd (Cwd view) ghcjsCmd)

        <$> ghcjsOutputs

    mconcat $ (\ s ->
        out </> "task-view" </> s %> \ _ -> do
            need [ tmp </> "app.jsexe" </> s ]
            putNormal $ "# copy " ++ s
            copyFileChanged
                (tmp </> "app.jsexe" </> s)
                (out </> "task-view" </> s))
        <$> ghcjsOutputs

ghcjsCmd :: String
ghcjsCmd =
    [r|ghcjs 
    -DGHCJS_BROWSER 
    -XMonoLocalBinds
    -outputdir ../../__www-build/app.jsout 
    -o ../../__www-build/app.jsexe 
    App.hs|]
