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
import Development.Shake.FilePath ((</>))

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
cleanRules = do
    phony "clean" $ do
        need [ "clean-www", "clean-view-reflex" ]

    phony "clean-www" $ do
        removeFilesAfter "__www" [ "//*" ] 

    phony "clean-view-reflex" $ do
        removeFilesAfter ("view" </> "app.jsexe") ghcjsOutputs
        removeFilesAfter "view" ghcjsIntermediates

buildRules :: Rules ()
buildRules = do
    phony "view-www" $ do
        liftIO $ putStrLn $ "#phony view-www" 
        need $ (\ s -> "__www" </> "task-view" </> s)
             <$> [ "all.js", "app.html" ]

    phony "view-reflex" $ do
        liftIO $ putStrLn $ "#phony view-reflex" 
        need [ "view" </> "task.jsexe" </> "all.js" ]

    "view" </> "node_modules" </> "webpack" </> "package.json" %> \ _ -> do
        liftIO $ putStrLn $ "#install node_modules" 
        cmd (Cwd "view") Shell "yarn install"

    "__www" </> "task-view" </> "app.html" %> \ _ -> do
        liftIO $ putStrLn $ "#pack app.html" 
        need [ "view" </> "node_modules" </> "webpack" </> "package.json" ]
        cmd (Cwd "view") Shell "yarn run pack"

    mconcat $ (\ s -> do
        "view" </> "app.jsexe" </> s %> \ _ -> do
            need [ "view" </> "App.hs"
                 , "view" </> "FlareTiming" </> "Task.hs"
                 , "view" </> "FlareTiming" </> "Map.hs"
                 ]

            liftIO $ putStrLn $ "#compile " ++ s
            cmd (Cwd "view") "ghcjs -DGHCJS_BROWSER App.hs")
        <$> ghcjsOutputs

    mconcat $ (\ s -> do
        "__www" </> "task-view" </> s %> \ _ -> do
            need [ "view" </> "app.jsexe" </> s ]
            liftIO $ putStrLn $ "#copy " ++ s
            copyFileChanged
                ("view" </> "app.jsexe" </> s)
                ("__www" </> "task-view" </> s))
        <$> ghcjsOutputs
