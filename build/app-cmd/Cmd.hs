module Cmd
    ( buildRules
    , cleanRules
    , testRules
    , lintRules
    , docTestRules
    ) where

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

buildTestName :: (Pkg, Test) -> String
buildTestName (pkg, suite) = "-build-test-" ++ pkg ++ ":" ++ suite

testName :: (Pkg, Test) -> String
testName (pkg, suite) = "-test-" ++ pkg ++ ":" ++ suite

docTestName :: Pkg -> String
docTestName pkg = "-doctest-" ++ pkg

tooling :: Tooling -> String
tooling CabalTooling = "cabal"
tooling StackTooling = "stack"

cmdDocTestFor :: Tooling -> String -> String
cmdDocTestFor CabalTooling x = "cabal new-test " ++ x ++ ":doctest"
cmdDocTestFor StackTooling x = "stack test " ++ x ++ ":doctest"

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

cmdBuildTestFor :: Tooling -> String -> String
cmdBuildTestFor CabalTooling x = "cabal new-build " ++ x
cmdBuildTestFor StackTooling x = "stack build " ++ x ++ " --no-run-tests"

type Pkg = String
type Test = String

-- | The names of the library packages.
pkgs :: [Pkg]
pkgs =
    [ "detour-via-sci"
    , "detour-via-uom"
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
    [ ("siggy-chardust", "digits")
    , ("flight-comp", "comp")
    , ("flight-earth", "earth")
    , ("flight-gap", "score")
    , ("flight-fsdb", "parse")
    , ("flight-kml", "parse")
    , ("flight-task", "task")
    ] 

-- | The pairs are names of the pkg and test.
docTestPkgs :: [Pkg]
docTestPkgs =
    [ "detour-via-sci"
    , "detour-via-uom"
    , "flight-kml"
    , "siggy-chardust"
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
    phony "clean-cabal-files" $
        removeFilesAfter "." ["//*.cabal"]

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
    sequence_ $ lintWithRule t' <$> flyPkgs

    phony (t ++ "-lint") $ need
        $ (t ++ "-lint-build")
        : (t ++ "-lint-detour-via-sci")
        : (t ++ "-lint-detour-via-uom")
        : (t ++ "-lint-siggy-chardust")
        : (t ++ "-lint-tasty-compare")
        : (t ++ "-lint-flare-timing")
        : (prefix (t ++ "-lint-") <$> flyPkgs)

    phony (t ++ "-lint-detour-via-sci") $
        cmd
            (Cwd "detour-via-sci")
            Shell
            (cmdTestFor t' "detour-via-sci:hlint")

    phony (t ++ "-lint-detour-via-uom") $
        cmd
            (Cwd "detour-via-uom")
            Shell
            (cmdTestFor t' "detour-via-uom:hlint")

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

docTestRule :: Tooling -> Pkg -> Rules ()
docTestRule t' pkg = do
    let t = tooling t'

    phony (t ++ buildTestName (pkg, "doctest")) $
        cmd
            Shell
            (cmdBuildTestFor t' $ pkg ++ ":" ++ "doctest")

    phony (t ++ docTestName pkg) $
        cmd
            Shell
            (cmdDocTestFor t' pkg)

docTestWithRules :: Tooling -> Rules ()
docTestWithRules t' = do
    let t = tooling t'
    sequence_ $ docTestRule t' <$> docTestPkgs

    phony (t ++ "-doctest") $ need $ docTestName <$> docTestPkgs

docTestRules :: Rules ()
docTestRules = do
    docTestWithRules CabalTooling
    docTestWithRules StackTooling

testRule :: Tooling -> (Pkg, Test) -> Rules ()
testRule t' x@(pkg, test) = do
    let t = tooling t'

    phony (t ++ buildTestName x) $
        cmd
            Shell
            (cmdBuildTestFor t' $ pkg ++ ":" ++ test)

    phony (t ++ testName x) $
        cmd
            Shell
            (cmdTestFor t' $ pkg ++ ":" ++ test)

testWithRules :: Tooling -> Rules ()
testWithRules t' = do
    let t = tooling t'
    sequence_ $ testRule t' <$> testPkgs

    phony (t ++ "-test") $ need $ buildTestName <$> testPkgs

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
            Shell
            (cmdBuildFor t' $ project ++ ":" ++ s)

buildWithRules :: Tooling -> Rules ()
buildWithRules t' = do
    sequence_ $ (\s -> buildRule t' s Nothing) <$> pkgs

    sequence_
        $ buildRule t' "flare-timing"
        <$> (Just <$> (testApps ++ prodApps))

    sequence_ $ buildRule t' "www" <$> (Just <$> wwwApps)

    phony (t ++ "-test-apps") $ need $ f <$> testApps
    phony (t ++ "-prod-apps") $ need $ f <$> prodApps
    phony (t ++ "-www-apps") $ need $ f <$> wwwApps
    phony (t ++ "-test-suites") $ need $ g <$> testPkgs
    phony (t ++ "-doctest-suites") $ need $ h <$> docTestPkgs
    
    where
        t = tooling t'
        f s = t ++ "-flare-timing-" ++ s
        g x = t ++ buildTestName x
        h x = t ++ docTestName x

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
