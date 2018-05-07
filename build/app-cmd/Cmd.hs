module Cmd (buildRules, cleanRules, testRules, lintRules) where

import Development.Shake
    ( Rules
    , CmdOption(Shell, Cwd)
    , removeFilesAfter
    , phony
    , cmd
    , need
    )
import Nix (flyPkgs, prefix)

data Tooling = CabalTooling | StackTooling

type CabalProject = String
type CabalTarget = String

tooling :: Tooling -> String
tooling CabalTooling = "cabal"
tooling StackTooling = "stack"

cmdTestFor :: Tooling -> String -> String
cmdTestFor CabalTooling x = "cabal new-test " ++ x
cmdTestFor StackTooling x = "stack test " ++ x

-- SEE: https://lexi-lambda.github.io/blog/2018/02/10/an-opinionated-guide-to-haskell-in-2018/
compilerToolFor :: Tooling -> String -> String
compilerToolFor CabalTooling _ = ""
compilerToolFor StackTooling x = "stack build --copy-compiler-tool " ++ x

cmdBuildFor :: Tooling -> String -> String
cmdBuildFor CabalTooling x = "cabal new-build " ++ x
cmdBuildFor StackTooling x = "stack build " ++ x ++ " --copy-bins"

type Pkg = String
type Test = String

-- | The names of the library packages.
pkgs :: [Pkg]
pkgs =
    [ "aeson-via-sci"
    , "aeson-via-uom"
    , "flight-cmd"
    , "flight-comp"
    , "flight-earth"
    , "flight-fsdb"
    , "flight-gap"
    , "flight-igc"
    , "flight-kml"
    , "flight-latlng"
    , "flight-lookup"
    , "flight-mask"
    , "flight-route"
    , "flight-scribe"
    , "siggy-chardust"
    , "flight-span"
    , "flight-task"
    , "tasty-compare"
    , "flight-track"
    , "flight-units"
    , "flight-zone"
    ]

-- | The pairs are names of the pkg and test.
testPkgs :: [(Pkg, Test)]
testPkgs =
    [ ("task", "task")
    , ("fsdb", "parse")
    , ("gap", "score")
    , ("kml", "parse")
    ] 

-- | The names of the test app executables.
testApps :: [String]
testApps =
    [ "test-fsdb-parser"
    , "test-igc-parser"
    , "test-kml-parser"
    ] 

-- | The names of the production app executables
prodApps :: [String]
prodApps =
    [ "extract-input"
    , "task-length"
    , "cross-zone"
    , "tag-zone"
    , "align-time"
    , "discard-further"
    , "mask-track"
    , "land-out"
    , "gap-point"
    ] 

-- | The names of the production app executables
wwwApps :: [String]
wwwApps =
    [ "comp-serve"
    ] 

cleanRules :: Rules ()
cleanRules = do
    phony "clean-prod-apps" $
        removeFilesAfter "__shake-build" prodApps

    phony "clean-test-apps" $
        removeFilesAfter "__shake-build" testApps

lintWithRule :: Tooling -> String -> Rules ()
lintWithRule t' s = do
    let t = tooling t'
    phony (t ++ "-lint-" ++ s) $
        cmd
            (Cwd s) 
            Shell
            (cmdTestFor t' $ "flight-" ++ s ++ ":hlint")

lintWithRules :: Tooling -> Rules ()
lintWithRules t' = do
    let t = tooling t'
    _ <- sequence_ $ lintWithRule t' <$> flyPkgs

    phony (t ++ "-lint") $ need
        $ (t ++ "-lint-build")
        : (t ++ "-lint-aeson-via-sci")
        : (t ++ "-lint-aeson-via-uom")
        : (t ++ "-lint-siggy-chardust")
        : (t ++ "-lint-tasty-compare")
        : (t ++ "-lint-flare-timing")
        : (prefix (t ++ "-lint-") <$> flyPkgs)

    phony (t ++ "-lint-aeson-via-sci") $
        cmd
            (Cwd "aeson-via-sci")
            Shell
            (cmdTestFor t' "aeson-via-sci:hlint")

    phony (t ++ "-lint-aeson-via-uom") $
        cmd
            (Cwd "aeson-via-uom")
            Shell
            (cmdTestFor t' "aeson-via-uom:hlint")

    phony (t ++ "-lint-siggy-chardust") $
        cmd
            (Cwd "siggy-chardust")
            Shell
            (cmdTestFor t' "siggy-chardust:hlint")

    phony (t ++ "-lint-tasty-compare") $
        cmd
            (Cwd "tasty-compare")
            Shell
            (cmdTestFor t' "tasty-compare:hlint")

    phony (t ++ "-lint-build") $
        cmd
            (Cwd "build")
            Shell
            (cmdTestFor t' "build-flare-timing:hlint")

    phony (t ++ "-lint-flare-timing") $
        cmd
            (Cwd "flare-timing")
            Shell
            (cmdTestFor t' "flare-timing:hlint")

lintRules :: Rules ()
lintRules = do
    lintWithRules CabalTooling
    lintWithRules StackTooling

testRule :: Tooling -> (Pkg, Test) -> Rules ()
testRule t' (pkg, test) = do
    let t = tooling t'
    phony (t ++ "-test-" ++ pkg) $
        cmd
            (Cwd pkg)
            Shell
            (cmdTestFor t' $ "flight-" ++ pkg ++":" ++ test)

testWithRules :: Tooling -> Rules ()
testWithRules t' = do
    let t = tooling t'
    _ <- sequence_ $ testRule t' <$> testPkgs

    phony (t ++ "-test") $
        need $ prefix (t ++ "-test-") . fst <$> testPkgs

testRules :: Rules ()
testRules = do
    testWithRules CabalTooling
    testWithRules StackTooling

buildRule :: Tooling -> CabalProject -> Maybe CabalTarget -> Rules ()

buildRule t' project Nothing = do
    let t = tooling t'
    phony (t ++ "-" ++ project) $
        cmd
            Shell
            (cmdBuildFor t' project)

buildRule t' project (Just s) = do
    let t = tooling t'
    phony (t ++ "-" ++ project ++ "-" ++ s) $
        cmd
            (Cwd project)
            Shell
            (cmdBuildFor t' $ project ++ ":" ++ s)

buildWithRules :: Tooling -> Rules ()
buildWithRules t' = do
    _ <- sequence_
            $ (\s -> buildRule t' s Nothing) <$> pkgs

    _ <- sequence_
            $ buildRule t' "flare-timing"
            <$> (Just <$> (testApps ++ prodApps))

    _ <- sequence_
            $ buildRule t' "www"
            <$> (Just <$> wwwApps)

    phony (t ++ "-test-apps") $ need $ f <$> testApps
    phony (t ++ "-prod-apps") $ need $ f <$> prodApps
    phony (t ++ "-www-apps") $ need $ f <$> wwwApps
    
    where
        t = tooling t'
        f s = t ++ "-" ++ s

buildRules :: Rules ()
buildRules = do
    buildWithRules CabalTooling
    buildWithRules StackTooling

    phony "weeder" $
        cmd
            (Cwd "flare-timing")
            Shell
            (compilerToolFor StackTooling "weeder")

    phony "hoogle" $
        cmd
            (Cwd "flare-timing")
            Shell
            (compilerToolFor StackTooling "hoogle")
