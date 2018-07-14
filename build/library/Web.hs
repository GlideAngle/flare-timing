module Web (buildRules, cleanRules) where

import Development.Shake
    ( Rules
    , CmdOption(Cwd, Shell)
    , (%>)
    , removeFilesAfter
    , phony
    , cmd
    , need
    , copyFileChanged
    , liftIO
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

ghcjsIntermediates :: [ String ]
ghcjsIntermediates =
    [ "//*.js_hi"
    , "//*.js_o"
    , "//*.js_dyn_hi"
    , "//*.js_dyn_o"
    ]

cleanRules :: Rules ()
cleanRules =
    phony "clean-www" $ do
        removeFilesAfter out [ "//*" ] 
        removeFilesAfter tmp [ "//*" ]

root :: FilePath
root = "flare-timing"

out :: FilePath
out = "__www-dist"

tmp :: FilePath
tmp = "__www-build"

view :: FilePath
view = root </> "view"

buildRules :: Rules ()
buildRules = do
    phony "view-start" $
        cmd (Cwd view) Shell "yarn run start"

    phony "view-www" $ do
        liftIO $ putStrLn "#phony view-www" 
        need $ (\ s -> out </> "task-view" </> s)
             <$> [ "all.js", "app.html" ]

    phony "view-reflex" $ do
        liftIO $ putStrLn "#phony view-reflex" 
        need [ out </> "task.jsexe" </> "all.js" ]

    root </> "view" </> "node_modules" </> "webpack" </> "package.json" %> \ _ -> do
        liftIO $ putStrLn "#install node_modules" 
        cmd (Cwd view) Shell "yarn install"

    out </> "task-view" </> "app.html" %> \ _ -> do
        liftIO $ putStrLn "#pack app.html" 
        need [ root </> "view" </> "node_modules" </> "webpack" </> "package.json" ]
        cmd (Cwd view) Shell "yarn run pack"

    mconcat $ (\ s ->
        tmp </> "app.jsexe" </> s %> \ _ -> do
            need [ root </> "view" </> "App.hs"
                 , root </> "view" </> "FlareTiming" </> "Task.hs"
                 , root </> "view" </> "FlareTiming" </> "Map.hs"
                 ]

            liftIO $ putStrLn $ "#compile " ++ s
            cmd (Cwd view) "ghcjs -DGHCJS_BROWSER -outputdir ../../__www-build/app.jsout -o ../../__www-build/app.jsexe App.hs")
        <$> ghcjsOutputs

    mconcat $ (\ s ->
        out </> "task-view" </> s %> \ _ -> do
            need [ tmp </> "app.jsexe" </> s ]
            liftIO $ putStrLn $ "#copy " ++ s
            copyFileChanged
                (tmp </> "app.jsexe" </> s)
                (out </> "task-view" </> s))
        <$> ghcjsOutputs
