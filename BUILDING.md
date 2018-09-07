# Building Flare Timing

This mono-repo contains many packages. The subset of these that are generally
useful are published to hackage and stackage.

## Building with Nix
Each library package can be built standalone. For example, the `flight-units`
package can be built with
[nix-build](https://nixos.org/nix/manual/#sec-building-simple) after having
setup the [overlay](https://github.com/BlockScope/nix-config).

    ln -s -f ~/dev/blockscope/nix-config/overlays.nix ~/.config/nixpkgs/overlays/flare-timing-overlay.nix
    nix-build "<nixpkgs>" -A haskellPackages.flight-units

## Building with Stack

The project as a whole can be built with [`stack
build`](https://docs.haskellstack.org) and executables installed with;

    flare-timing> stack install
    ...
    Copied executables to /.../flare-timing/__shake-build:
    - align-time
    - build-flare-timing
    - cross-zone
    - discard-further
    - extract-input
    - gap-point
    - land-out
    - make-travis-yml
    - mask-track
    - tag-zone
    - task-length
    - test-fsdb-parser
    - test-igc-parser
    - test-kml-parser

Individual packages can be built by specifying either the folder or the package
name;

    flare-timing> stack build units
    flare-timing> stack build flight-units
    
## Building with Cabal

As we're depending on some git packages, draw these down using
[stack2cabal](https://github.com/brunjlar/stack2cabal);

    flare-timing> stack install stack2cabal
    flare-timing> stack exec stack2cabal -- .
    ...
    flare-timing> cabal new-build all
    cabal: Could not resolve dependencies:
    [__0] trying: aeson-via-sci-0.1.0 (user goal)
    [__1] trying: template-haskell-2.13.0.0/installed-2.1... (dependency of
    aeson-via-sci)
    [__2] next goal: uom-plugin (user goal)
    [__2] rejecting: uom-plugin-0.2.0.1 (conflict:
    template-haskell==2.13.0.0/installed-2.1..., uom-plugin =>
    template-haskell>=2.9 && <2.13)
    [__2] rejecting: uom-plugin-0.2.0.0, uom-plugin-0.1.1.0, uom-plugin-0.1.0.0
    (constraint from user target requires ==0.2.0.1)
    After searching the rest of the dependency tree exhaustively, these were the
    goals I've had most trouble fulfilling: template-haskell, uom-plugin,
    aeson-via-sci

If the build is working with `stack` then we might be able to get it to work
with `cabal`;

    > mv cabal.project __cabal.project
    > stack exec stack2cabal -- .
    > mv cabal.project cabal.project.local
    > mv __cabal.project cabal.project

Edit `cabal.project.local` so that it looks something like;

    with-compiler:
        /Users/.../.stack/programs/x86_64-osx/ghc-8.2.2/bin/ghc
    constraints:
        Cabal == 2.0.1.1
        , Only == 0.1
        , QuickCheck == 2.10.1
        ...
        , void == 0.7.2
        , yaml == 0.8.29

Now let's do the build again;

    > cabal new-build all
    Build profile: -w ghc-8.2.2 -O1
    In order, the following will be built (use -v for more details):
    ...
    > cabal new-build all
    Up to date

## Building with Pier

```
> stack install pier --stack-yaml=stack-pier.yaml
> stack exec pier -- test flight-fsdb:test-suite:parse
> stack exec pier -- build flare-timing:exe
```

## Building with Shake

Tasks that are not simple by hand have been added to the shake build project
[build-flare-timing](build).

### Generating `*.cabal` files

The `*.cabal` files are generated using
[hpack-dhall](https://github.com/sol/hpack-dhall) and there's a shake build
target setup for doing this and formatting the `package.dhall` files but first
both `dhall` and `hpack-dhall` need to be installed.

```
> stack install dhall hpack-dhall --stack-yaml=stack-dhall.yaml
> ./stack-shake-build.sh cabal-files
```

There are three shell scripts for building the shake build using `stack`,
`cabal` or `pier`;

* `stack-shake-build.sh`
* `cabal-shake-build.sh`
* `pier-shake-build.sh`

Any of these can be used to call further steps relying on either `stack`, `cabal` or
`pier`;

```
> ./stack-shake-build.sh stack-lint-kml
> ./stack-shake-build.sh cabal-lint-kml
> ./cabal-shake-build.sh stack-lint-kml
> ./cabal-shake-build.sh cabal-lint-kml
> ./pier-shake-build.sh pier-prod-apps
> ./pier-shake-build.sh pier-test-flight-fsdb:parse
> ./stack-shake-build.sh stack-test-flight-fsdb:parse
> ./cabal-shake-build.sh cabal-test-flight-fsdb:parse
```

### Running `doctest` Tests

There's a target for building all `doctest` tests;

```
> ./stack-shake-build.sh stack-doctest
# stack (for stack-doctest-siggy-chardust)
siggy-chardust-1.0.0: test (suite: doctest)

Examples: 35  Tried: 35  Errors: 0  Failures: 0

siggy-chardust-1.0.0: Test suite doctest passed
# stack (for stack-doctest-flight-kml)
flight-kml-1.0.0: test (suite: doctest)

Examples: 57  Tried: 57  Errors: 0  Failures: 0

flight-kml-1.0.0: Test suite doctest passed
# stack (for stack-doctest-detour-via-uom)
detour-via-uom-1.0.0: test (suite: doctest)

Examples: 27  Tried: 27  Errors: 0  Failures: 0

detour-via-uom-1.0.0: Test suite doctest passed
# stack (for stack-doctest-detour-via-sci)
detour-via-sci-1.0.0: test (suite: doctest)

Examples: 44  Tried: 44  Errors: 0  Failures: 0

detour-via-sci-1.0.0: Test suite doctest passed
Build completed in 0:21m
```

The `doctest` targets can be run individually too;

```
> ./stack-shake-build.sh stack-doctest-flight-kml
...
# stack (for stack-doctest-flight-kml)
flight-kml-1.0.0: test (suite: doctest)

Examples: 57  Tried: 57  Errors: 0  Failures: 0

flight-kml-1.0.0: Test suite doctest passed
Build completed in 0:09m
```

```
> ./cabal-shake-build.sh cabal-doctest-flight-kml
...
# cabal (for cabal-doctest-flight-kml)
Running 1 test suites...
Test suite doctest: RUNNING...
Examples: 57  Tried: 57  Errors: 0  Failures: 0
Test suite doctest: PASS
Test suite logged to:
/.../dist-newstyle/build/x86_64-osx/ghc-8.2.2/flight-kml-1.0.0/t/doctest/test/flight-kml-1.0.0-doctest.log
1 of 1 test suites (1 of 1 test cases) passed.
Build completed in 0:22m
```

## Library Packages

For handling decimal places and significant digits;
* [`detour-via-sci`](detour-via-sci)
* [`detour-via-uom`](detour-via-uom)
* [`siggy-chardust`](siggy-chardust)

Units, latitudes, longitudes and distances on Earth;
* [`flight-units`](units)
* [`flight-latlng`](latlng)
* [`flight-earth`](earth)

Parsing input file formats;
* [`flight-fsdb`](fsdb)
* [`flight-igc`](igc)
* [`flight-kml`](kml)

Command line inputs;
* [`flight-cmd`](cmd)
* [`flight-span`](span)

The competition and scoring;
* [`flight-comp`](comp)
* [`flight-gap`](gap)
* [`flight-lookup`](lookup)
* [`flight-mask`](mask)
* [`flight-task`](task)
* [`flight-route`](route)

To do with files read from and written to during scoring;
* [`flight-scribe`](scribe)
* [`flight-track`](track)

Testing;
* [`tasty-compare`](tasty-compare)

The command line programs for scoring are in `./flare-timing`.
