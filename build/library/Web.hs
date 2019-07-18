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
import qualified Data.Text as T (Text, pack, unpack)

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

outCabal :: FilePath
outCabal = "__www-dist-cabal" </> "task-view"

outGhcjs :: FilePath
outGhcjs = "__www-dist-ghcjs" </> "task-view"

tmpCabal :: FilePath
tmpCabal =
    "dist-ghcjs/build/x86_64-linux/ghcjs-8.4.0.1/app-view-0.1.0/x/comp-view/build/comp-view"
    </> "comp-view.jsexe"

tmpGhcjs :: FilePath
tmpGhcjs = "__www-build-ghcjs" </> "app.jsexe"

view :: FilePath
view = "app-view" </> "comp-view"

webpackPackageJson :: FilePath
webpackPackageJson = view </> "node_modules" </> "webpack" </> "package.json"

type Suffix = String
type TmpDir = FilePath
type OutDir = FilePath

buildWith :: Suffix -> TmpDir -> OutDir -> Rules ()
buildWith suffix tmpdir outdir = do
    phony ("view-start-" ++ suffix) $ do
        need [ outdir </> "app.html" ]
        cmd (Cwd view) Shell $ "yarn run start-" ++ suffix

    phony ("view-www-" ++ "suffix") $ need [ outdir </> "all.js" ]

    outdir </> "app.html" %> \ _ -> do
        putNormal "# pack app.html" 
        need [ webpackPackageJson, outdir </> "all.js" ]
        cmd (Cwd view) Shell $ "yarn run pack-" ++ suffix

    mconcat $ (\s -> outdir </> s %> \ _ -> do
        need [ tmpdir </> s ]
        putNormal $ "# copy " ++ s
        copyFileChanged (tmpdir </> s) (outdir </> s))
        <$> ghcjsOutputs

buildWithGhcjsRules :: Rules ()
buildWithGhcjsRules = do
    buildWith "ghcjs" tmpGhcjs outGhcjs

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
    buildWith "cabal" tmpCabal outCabal

    mconcat $ (\s -> tmpCabal </> s %> \ _ -> do
        cmd Shell "./cabal-ghcjs new-build app-view")
        <$> ghcjsOutputs

buildRules :: Rules ()
buildRules = do
    webpackPackageJson %> \ _ -> do
        putNormal "# install node_modules" 
        cmd (Cwd view) Shell "yarn install"

    buildWithGhcjsRules
    buildWithCabalRules
