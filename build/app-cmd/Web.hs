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

cleanRules :: Rules ()
cleanRules = do
    phony "clean-view-reflex" $ do
        removeFilesAfter ("view" </> "task.jsexe") ghcjsOutputs

buildRules :: Rules ()
buildRules = do
    phony "view-www" $ do
        liftIO $ putStrLn $ "#phony view-www" 
        need $ (\ s -> "__www" </> "task-view" </> s)
             <$> [ "all.js", "task.html" ]

    phony "view-reflex" $ do
        liftIO $ putStrLn $ "#phony view-reflex" 
        need [ "view" </> "task.jsexe" </> "all.js" ]

    "view" </> "node_modules" </> "webpack" </> "package.json" %> \ _ -> do
        liftIO $ putStrLn $ "#install node_modules" 
        cmd (Cwd "view") Shell "yarn install"

    "__www" </> "task-view" </> "task.html" %> \ _ -> do
        liftIO $ putStrLn $ "#pack task.html" 
        need [ "view" </> "node_modules" </> "webpack" </> "package.json" ]
        cmd (Cwd "view") Shell "yarn run pack"

    mconcat $ (\ s -> do
        "view" </> "task.jsexe" </> s %> \ _ -> do
            need [ "view" </> "task.hs" ]
            liftIO $ putStrLn $ "#compile " ++ s
            cmd (Cwd "view") "ghcjs -DGHCJS_BROWSER task.hs")
        <$> ghcjsOutputs

    mconcat $ (\ s -> do
        "__www" </> "task-view" </> s %> \ _ -> do
            need [ "view" </> "task.jsexe" </> s ]
            liftIO $ putStrLn $ "#copy " ++ s
            copyFileChanged
                ("view" </> "task.jsexe" </> s)
                ("__www" </> "task-view" </> s))
        <$> ghcjsOutputs
