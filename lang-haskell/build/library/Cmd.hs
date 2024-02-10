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

data Tooling
    = CabalTooling
    | StackTooling
    | PierTooling

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
tooling PierTooling = "pier"

cmdDocTestFor :: Tooling -> String -> String
cmdDocTestFor CabalTooling x = "cabal v2-test " ++ x ++ ":doctest"
cmdDocTestFor StackTooling x = "stack test " ++ x ++ ":doctest"
cmdDocTestFor PierTooling x = "stack exec pier -- test " ++ x ++ ":doctest"

cmdTestFor :: Tooling -> (Pkg, Test) -> String
cmdTestFor CabalTooling (x, y) = "cabal v2-test " ++ x ++ ":" ++ y
cmdTestFor StackTooling (x, y) = "stack test " ++ x ++ ":" ++ y
cmdTestFor PierTooling (x, y) = "stack exec pier -- test " ++ x ++ ":test-suite:" ++ y

-- SEE: https://lexi-lambda.github.io/blog/2018/02/10/an-opinionated-guide-to-haskell-in-2018/
compilerToolFor :: Tooling -> String -> String
compilerToolFor CabalTooling _ = ""
compilerToolFor StackTooling x = "stack build --copy-compiler-tool " ++ x
compilerToolFor PierTooling _ = ""

cmdBuildFor :: Tooling -> String -> String
cmdBuildFor CabalTooling x = "cabal v2-build " ++ x
cmdBuildFor StackTooling x = "stack build " ++ x ++ " --copy-bins"
cmdBuildFor PierTooling x = "stack exec pier -- build " ++ x

cmdBuildTestFor :: Tooling -> String -> String
cmdBuildTestFor CabalTooling x = "cabal v2-build " ++ x
cmdBuildTestFor StackTooling x = "stack build " ++ x ++ " --no-run-tests"
cmdBuildTestFor PierTooling x = "stack exec pier -- build " ++ x ++ " --no-run-tests"

type Pkg = String
type Test = String

-- | The names of the library packages.
pkgs :: [Pkg]
pkgs =
    [ "detour-via-sci"
    , "detour-via-uom"
    , "flight-clip"
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
    , "flight-time"
    , "flight-track"
    , "flight-units"
    , "flight-zone"
    , "tasty-compare"
    ]

-- | The pairs are names of the pkg and test.
testPkgs :: [(Pkg, Test)]
testPkgs =
    [ ("siggy-chardust", "digits")
    -- TODO: Get flight-comp:test:comp compiling again.
    -- , ("flight-comp", "comp")
    , ("flight-earth", "geodesy")
    , ("flight-earth", "forbes")
    , ("flight-earth", "forbes-r")
    , ("flight-earth", "greda")
    , ("flight-earth", "greda-r")
    , ("flight-earth", "meridian")
    , ("flight-earth", "meridian-r")
    , ("flight-earth", "published")
    , ("flight-earth", "published-r")
    , ("flight-kml", "parse")
    -- TODO: Get flight-task:test:task compiling again.
    -- , ("flight-task", "task")
    ]

-- | The pairs are names of the pkg and test.
docTestPkgs :: [Pkg]
docTestPkgs =
    [ "detour-via-sci"
    -- TODO: Investigate why detour-via-uom doctests are failing with the u quasiquote.
    -- , "detour-via-uom"
    , "siggy-chardust"
    , "flight-clip"
    , "flight-comp"
    , "flight-kml"
    , "flight-igc"
    , "flight-track"
    ]

-- | The names of the test app executables.
testApps :: [String]
testApps =
    [
    -- "ft-fsdb-parser"
    --, "ft-igc-parser"
    --, "ft-kml-parser"
    ]

-- | The names of the production app executables
prodApps :: [String]
prodApps =
    [ "fs-route"
    , "fs-arrival"
    , "fs-effort"
    , "fs-clean"
    , "fs-trim"
    , "fs-score"

    , "ft-extract-input"
    , "ft-task-length"
    , "ft-cross-zone"
    , "ft-tag-zone"
    , "ft-peg-frame"
    , "ft-align-time"
    , "ft-discard-further"
    , "ft-mask-arrival"
    , "ft-mask-bonus"
    , "ft-mask-lead"
    , "ft-mask-effort"
    , "ft-land-out"
    , "ft-gap-point"
    ]

-- | The names of the production app executables
wwwApps :: [String]
wwwApps =
    [ "ft-comp-serve"
    ]

cleanRules :: Rules ()
cleanRules = do
    phony "clean-cabal-files" $
        removeFilesAfter "." ["//*.cabal"]

lintWithRule :: Tooling -> String -> Rules ()
lintWithRule t' s = do
    let t = tooling t'
    phony (t ++ "-lint-" ++ s) $
        cmd
            (Cwd s) 
            Shell
            (cmdTestFor t' ("flight-" ++ s, "hlint"))

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
            (cmdTestFor t' ("detour-via-sci", "hlint"))

    phony (t ++ "-lint-detour-via-uom") $
        cmd
            (Cwd "detour-via-uom")
            Shell
            (cmdTestFor t' ("detour-via-uom", "hlint"))

    phony (t ++ "-lint-siggy-chardust") $
        cmd
            (Cwd "siggy-chardust")
            Shell
            (cmdTestFor t' ("siggy-chardust", "hlint"))

    phony (t ++ "-lint-tasty-compare") $
        cmd
            (Cwd "tasty-compare")
            Shell
            (cmdTestFor t' ("tasty-compare", "hlint"))

    phony (t ++ "-lint-build") $
        cmd
            (Cwd "build")
            Shell
            (cmdTestFor t' ("build-flare-timing", "hlint"))

    phony (t ++ "-lint-flare-timing") $
        cmd
            (Cwd "flare-timing")
            Shell
            (cmdTestFor t' ("flare-timing", "hlint"))

lintRules :: Rules ()
lintRules = do
    lintWithRules CabalTooling
    lintWithRules StackTooling
    lintWithRules PierTooling

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

    phony (t ++ "-doctest") $ need $ (t ++) . docTestName <$> docTestPkgs

docTestRules :: Rules ()
docTestRules = do
    docTestWithRules CabalTooling
    docTestWithRules StackTooling
    docTestWithRules PierTooling

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
            (cmdTestFor t' (pkg, test))

testWithRules :: Tooling -> Rules ()
testWithRules t' = do
    let t = tooling t'
    sequence_ $ testRule t' <$> testPkgs

    phony (t ++ "-test") $ need $ buildTestName <$> testPkgs

testRules :: Rules ()
testRules = do
    testWithRules CabalTooling
    testWithRules StackTooling
    testWithRules PierTooling

buildRule :: Tooling -> CabalProject -> Maybe CabalTarget -> Rules ()

buildRule t'@PierTooling project (Just s) = do
    let t = tooling t'
    phony (t ++ "-" ++ project ++ "-" ++ s) $
        cmd
            Shell
            (cmdBuildFor t' $ project ++ ":exe:" ++ s)

buildRule t' project (Just s) = do
    let t = tooling t'
    phony (t ++ "-" ++ project ++ "-" ++ s) $
        cmd
            Shell
            (cmdBuildFor t' $ project ++ ":" ++ s)

buildRule t' project Nothing = do
    let t = tooling t'
    phony (t ++ "-" ++ project) $
        cmd
            Shell
            (cmdBuildFor t' project)

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
    buildWithRules PierTooling

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
