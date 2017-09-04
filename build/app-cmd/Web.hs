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
cleanRules = do
    phony "clean" $
        need [ "clean-www", "clean-view-reflex" ]

    phony "clean-www" $
        removeFilesAfter (root </> "__www") [ "//*" ] 

    phony "clean-view-reflex" $ do
        removeFilesAfter (root </> "view" </> "app.jsout") [ "//*" ]
        removeFilesAfter (root </> "view" </> "app.jsexe") [ "//*" ]
        removeFilesAfter (root </> "view") ghcjsIntermediates

root :: FilePath
root = "flare-timing"

view :: FilePath
view = root </> "view"

buildRules :: Rules ()
buildRules = do
    phony "view-start" $
        cmd (Cwd view) Shell "yarn run start"

    phony "view-www" $ do
        liftIO $ putStrLn "#phony view-www" 
        need $ (\ s -> root </> "__www" </> "task-view" </> s)
             <$> [ "all.js", "app.html" ]

    phony "view-reflex" $ do
        liftIO $ putStrLn "#phony view-reflex" 
        need [ root </> "view" </> "task.jsexe" </> "all.js" ]

    root </> "view" </> "node_modules" </> "webpack" </> "package.json" %> \ _ -> do
        liftIO $ putStrLn "#install node_modules" 
        cmd (Cwd view) Shell "yarn install"

    root </> "__www" </> "task-view" </> "app.html" %> \ _ -> do
        liftIO $ putStrLn "#pack app.html" 
        need [ root </> "view" </> "node_modules" </> "webpack" </> "package.json" ]
        cmd (Cwd view) Shell "yarn run pack"

    mconcat $ (\ s ->
        root </> "view" </> "app.jsexe" </> s %> \ _ -> do
            need [ root </> "view" </> "App.hs"
                 , root </> "view" </> "FlareTiming" </> "Task.hs"
                 , root </> "view" </> "FlareTiming" </> "Map.hs"
                 ]

            liftIO $ putStrLn $ "#compile " ++ s
            cmd (Cwd view) "ghcjs -DGHCJS_BROWSER -outputdir app.jsout App.hs")
        <$> ghcjsOutputs

    mconcat $ (\ s ->
        root </> "__www" </> "task-view" </> s %> \ _ -> do
            need [ root </> "view" </> "app.jsexe" </> s ]
            liftIO $ putStrLn $ "#copy " ++ s
            copyFileChanged
                (root </> "view" </> "app.jsexe" </> s)
                (root </> "__www" </> "task-view" </> s))
        <$> ghcjsOutputs
