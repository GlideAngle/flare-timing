{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}

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
    , liftIO
    )
import Development.Shake.FilePath (FilePath, (</>))
import GHC.Generics (Generic)
import Dhall
    ( Interpret, InterpretOptions(..)
    , defaultInterpretOptions, input, autoWith
    )
import qualified Data.Text.Lazy as T (Text, pack, unpack)

data DefaultExtensions =
    DefaultExtentions
        { defaultExtensions :: [T.Text] }
    deriving (Generic, Show)

instance Interpret DefaultExtensions

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
        removeFilesAfter "__www-build-ghcjs" [ "//*" ]
        removeFilesAfter "__www-dist-ghcjs" [ "//*" ]
        removeFilesAfter "__www-dist-cabal" [ "//*" ]

        removeFilesAfter
            "dist-ghcjs/build/x86_64-linux/ghcjs-8.4.0.1/app-view-0.1.0"
            [ "//*" ]


buildCabal :: FilePath
buildCabal = "dist-ghcjs/build/x86_64-linux/ghcjs-8.4.0.1/app-view-0.1.0/x/comp-view/build/comp-view"

outCabalWorking :: FilePath
outCabalWorking = buildCabal </> "comp-view.jsexe"

outCabal :: FilePath
outCabal = "__www-dist-cabal" </> "task-view"

outGhcjs :: FilePath
outGhcjs = "__www-dist-ghcjs" </> "task-view"

tmpGhcjs :: FilePath
tmpGhcjs = "__www-build-ghcjs" </> "app.jsexe"

view :: FilePath
view = "app-view" </> "comp-view"

webpackPackageJson :: FilePath
webpackPackageJson = view </> "node_modules" </> "webpack" </> "package.json"

buildWithGhcjsRules :: Rules ()
buildWithGhcjsRules = do
    phony "view-start-ghcjs" $ do
        need [ outGhcjs </> "app.html" ]
        cmd (Cwd view) Shell "yarn run start-ghcjs"

    phony "view-www-ghcjs" $ need [ outGhcjs </> "all.js" ]

    outGhcjs </> "app.html" %> \ _ -> do
        putNormal "# pack app.html" 
        need [ webpackPackageJson, outGhcjs </> "all.js" ]
        cmd (Cwd view) Shell "yarn run pack-ghcjs"

    mconcat $ (\ s -> outGhcjs </> s %> \ _ -> do
        need [ tmpGhcjs </> s ]
        putNormal $ "# copy " ++ s
        copyFileChanged (tmpGhcjs </> s) (outGhcjs </> s))
        <$> ghcjsOutputs

    mconcat $ (\s -> tmpGhcjs </> s %> \ _ -> do
        ghcjsCmd' <- liftIO ghcjsCmd
        putNormal ghcjsCmd'
        cmd (Cwd view) ghcjsCmd')
        <$> ghcjsOutputs
    where
        ghcjsCmd :: IO String
        ghcjsCmd = do
            exts <-
                input (autoWith interpretOpts)
                $ T.pack "./default-extensions-ghcjs.dhall"

            let xs = unwords $ ("-X" ++) . T.unpack <$> defaultExtensions exts

            return $ [r|ghcjs 
            -DGHCJS_BROWSER 
            |] ++ xs ++ [r|
            -Wall
            -outputdir ../../__www-build-ghcjs/app.jsout 
            -o ../../__www-build-ghcjs/app.jsexe 
            App.hs|]

        interpretOpts :: InterpretOptions
        interpretOpts =
            defaultInterpretOptions
                { fieldModifier = const $ T.pack "default-extensions" }


buildWithCabalRules :: Rules ()
buildWithCabalRules = do
    phony "view-start-cabal" $ do
        need [ outCabal </> "app.html" ]
        cmd (Cwd view) Shell "yarn run start-cabal"

    phony "view-www-cabal" $ need [ outCabalWorking </> "all.js" ]

    outCabal </> "app.html" %> \ _ -> do
        putNormal "# pack app.html" 
        need [ webpackPackageJson, outCabal </> "all.js" ]
        cmd (Cwd view) Shell "yarn run pack-cabal"

    mconcat $ (\s -> outCabal </> s %> \ _ -> do
        need [ outCabalWorking </> s ]
        putNormal $ "# copy " ++ s
        copyFileChanged (outCabalWorking </> s) (outCabal </> s))
        <$> ghcjsOutputs

    mconcat $ (\s -> outCabalWorking </> s %> \ _ -> do
        cmd Shell "./cabal-ghcjs new-build app-view")
        <$> ghcjsOutputs

buildRules :: Rules ()
buildRules = do
    webpackPackageJson %> \ _ -> do
        putNormal "# install node_modules" 
        cmd (Cwd view) Shell "yarn install"

    buildWithGhcjsRules
    buildWithCabalRules
