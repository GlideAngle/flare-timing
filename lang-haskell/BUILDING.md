# Building Flare Timing

This mono-repo contains many packages. The subset of these that are generally
useful are published to hackage and stackage.

## Building with Stack

The project as a whole can be built with [`stack
build`](https://docs.haskellstack.org) and executables installed with;

    flare-timing> stack install
    ...
    Copied executables to /Users/_/.local/bin:
    - align-time
    - build-flare-timing
    - comp-serve
    - cross-zone
    - discard-further
    - extract-input
    - fs-effort
    - fs-filter
    - fs-route
    - fs-score
    - gap-point
    - land-out
    - mask-track
    - peg-frame
    - tag-zone
    - task-length
    - test-fsdb-parser
    - test-igc-parser
    - test-kml-parser
    - unpack-track

## Building with Pier

```
> stack install pier --stack-yaml=stack-pier.yaml
> pier build
> pier run flare-timing:exe:fs-filter -- --help
> pier test siggy-chardust:test-suite:digits
```

## Building with Vernix
With vernix I can pin versions, jail break and skip docs and testing in the
`.vx2` setup, shown here with for `uom-plugin`.

```
HaskellPackage("uom-plugin", version = "0.3.0.0", dontCheck = True, jailBreak = True),
```

To regenerate `flare-timing-project.nix`;

```
> cd vernix
vernix> nix-shell
[nix-shell:~/.../flare-timing/vernix]$ ./vernix
Generating nix specifications from flare-timing.vx2 ...
Using here: .
..pkg flare-timing-project.nix as flare-timing-project from ./flare-timing-project.nix
.............................

Package     siggy-chardust: ../siggy-chardust
Package     detour-via-sci: ../detour-via-sci
Package     detour-via-uom: ../detour-via-uom
Package      tasty-compare: ../tasty-compare
Package        flight-clip: ../clip
Package         flight-cmd: ../cmd
Package        flight-comp: ../comp
Package       flight-earth: ../earth
Package        flight-fsdb: ../fsdb
Package         flight-gap: ../gap
Package         flight-igc: ../igc
Package         flight-kml: ../kml
Package      flight-latlng: ../latlng
Package      flight-lookup: ../lookup
Package        flight-mask: ../mask
Package       flight-route: ../route
Package      flight-scribe: ../scribe
Package        flight-span: ../span
Package        flight-task: ../task
Package        flight-time: ../time
Package       flight-track: ../track
Package       flight-units: ../units
Package        flight-zone: ../zone
Package       flare-timing: ../flare-timing
Package          app-serve: ../app-serve
Package             hcoord: 3c3859dac5da111e57a6de09764ffdb127197c4a https://github.com/blockscope/hcoord.git 3c3859dac5da111e57a6de09764ffdb127197c4a
Package         hcoord-utm: 3c3859dac5da111e57a6de09764ffdb127197c4a https://github.com/blockscope/hcoord.git 3c3859dac5da111e57a6de09764ffdb127197c4a
Package          hxt-xpath: 9.1.2.2 hackage
Package         uom-plugin: 0.3.0.0 hackage
Package            doctest: 0.15.0 hackage
Package         megaparsec: 7.0.4 hackage
Package parser-combinators: 1.0.0 hackage
Package         summarygen: ./flare-timing-project.nix
[nix-shell:~/.../flare-timing]$ exit
vernix>
```

To build a single package;

```
vernix> nix-build -A siggy-chardust flare-timing-project.nix
/nix/store/rcynlrd58dwq758gxkcb517p5hmryiqp-siggy-chardust-1.0.0
```

To build them all;

```
vernix> nix-build flare-timing-project.nix
/nix/store/sxfb6nkhvahngkxpydr3mknmk1fhlxlg-app-serve-0.1.0
/nix/store/s9savg6szykm7bb7cj5c93n63qf6pqiq-detour-via-sci-1.0.1
/nix/store/qwb2lfn0x5grb1d6fs1qyrsm0hdddima-detour-via-uom-1.0.1
/nix/store/h55yipm6w9i3mkh1w1a833nkr154m0mn-doctest-0.15.0
/nix/store/19q4sycdqrxzx1d9q0k0gr00jp1a6b8l-flare-timing-0.1.0
/nix/store/c8df4xyvl47ybgw85x0jv6h15sjkni40-flight-clip-1.1.0
/nix/store/a3w2r4q3wgfa2703iq53z3xnj204azmf-flight-cmd-0.1.0
/nix/store/fqvsimyx8k28yy0j84c780wzqb1phadg-flight-comp-0.1.0
/nix/store/8vla5l4lifbgw3a1d0i5wss33s0knqpi-flight-earth-0.1.0
/nix/store/d2p1cy1701rp2pfsgqr28kyv70c4y4ch-flight-fsdb-0.1.0
/nix/store/vs6sibri5xbk9wqnl8jn3cyz79xm0lnb-flight-gap-0.1.0
/nix/store/rwv7a4xjlp94l5rscrhi56pfn430ffp0-flight-igc-2.0.0
/nix/store/li9q6c5h3xblajw73pvd23azh0hnzhbs-flight-kml-1.1.0
/nix/store/aa2ig6y1n9nb432445nid10lrsm22pjp-flight-latlng-0.1.0
/nix/store/mh08dgx2xjccxjzyj5q18bxswsbisf8r-flight-lookup-0.1.0
/nix/store/am05dii1jrcv2pc7xy3lyjjbmfkzjsxn-flight-mask-0.1.0
/nix/store/78akv62nnb661v5i76zrz3m50bjd8s67-flight-route-0.1.0
/nix/store/6hnbmkfs2bs4xfij6h65lshwbzpl87xg-flight-scribe-0.1.0
/nix/store/kbz230px5ny2mkkbf78x7mw6iv36zy04-flight-span-0.1.0
/nix/store/1g6drpkwa4v25gp8bpl0irn9smcmhp2a-flight-task-0.1.0
/nix/store/3016d2za8m4nmdz2xwlz2h4k2fwyvsik-flight-time-0.1.0
/nix/store/qck7243g10lm8gaw674psnxxc6djysd8-flight-track-0.1.0
/nix/store/h0y870qhh1mch7fnc714x5c3hg318n4w-flight-units-0.1.0
/nix/store/7rbchfa287rg5ci5igsbzvjd6bkjszfj-flight-zone-0.1.0
/nix/store/hw77056rarpvf9wdy1zhdl5v3q9c8myd-hcoord-3c3859dac5da111e57a6de09764ffdb127197c4a
/nix/store/50azqjngn4n2akmnah144iagx3jsrz7f-hcoord-utm-3c3859dac5da111e57a6de09764ffdb127197c4a
/nix/store/55v4xzfa7a1wbx1kmympx190gwy8sxj9-hxt-xpath-9.1.2.2
/nix/store/gdzcsg7vnw002wxab9qlfsfhmbqmcn6g-megaparsec-7.0.4
/nix/store/7xg6scax0x9npmhhjpwjaayby6fcjv3h-parser-combinators-1.0.0
/nix/store/7y19jq5zl1wav2na2dsq4mvpqv7vb6v9-siggy-chardust-1.0.0
/nix/store/rwni563by4q7a922dk9acpv39qkczdhn-tasty-compare-0.1.0
/nix/store/79ragih9f2mdszjk8yj5ghydh7g4iy35-uom-plugin-0.3.0.0
```

## Building with Shake

Tasks that are not simple by hand have been added to the shake build project
[build-flare-timing](build).

### Generating `shell.nix` and `drv.nix` files
The `nix-shell` shake build rule ensures for each package that there's
a `shell.nix` and `drv.nix` with a nix derivation created with
[cabal2nix](https://github.com/NixOS/cabal2nix).

```
> stack install cabal2nix --stack-yaml=stack-cabal2nix.yaml
Copied executables to /Users/.../flare-timing/__shake-build:
- cabal2nix
- hackage2nix

> ./stack-shake-build.sh nix-shell

# cabal2nix (for zone/drv.nix)
# cabal2nix (for units/drv.nix)
# cabal2nix (for track/drv.nix)
# cabal2nix (for time/drv.nix)
# cabal2nix (for task/drv.nix)
# cabal2nix (for span/drv.nix)
# cabal2nix (for scribe/drv.nix)
# cabal2nix (for route/drv.nix)
# cabal2nix (for mask/drv.nix)
# cabal2nix (for lookup/drv.nix)
# cabal2nix (for latlng/drv.nix)
# cabal2nix (for kml/drv.nix)
# cabal2nix (for igc/drv.nix)
# cabal2nix (for gap/drv.nix)
# cabal2nix (for fsdb/drv.nix)
# cabal2nix (for earth/drv.nix)
# cabal2nix (for comp/drv.nix)
# cabal2nix (for cmd/drv.nix)
# cabal2nix (for clip/drv.nix)
# cabal2nix (for app-serve/drv.nix)
# cabal2nix (for flare-timing/drv.nix)
# cabal2nix (for tasty-compare/drv.nix)
# cabal2nix (for siggy-chardust/drv.nix)
# cabal2nix (for detour-via-uom/drv.nix)
# cabal2nix (for detour-via-sci/drv.nix)
Build completed in 0:01m
```

The `shell.nix` files are copied from `nix/hard-shell.nix`;

```
let
  config = import ../nix/config.nix {};
  pkgs = import ../nix/nixpkgs.nix { inherit config; };
in
  import ./drv.nix { nixpkgs = pkgs; }
```

### Generating `*.cabal` files

The `*.cabal` files are generated using
[hpack-dhall](https://github.com/sol/hpack-dhall) and there's a shake build
target setup for doing this and formatting the `package.dhall` files but first
both `dhall` and `hpack-dhall` need to be installed.

```
> stack install dhall hpack-dhall --stack-yaml=stack-dhall.yaml
Copied executables to /Users/.../flare-timing/__shake-build:
- dhall
- dhall-hpack-cabal
- dhall-hpack-dhall
- dhall-hpack-json
- dhall-hpack-yaml

> ./stack-shake-build.sh cabal-files
# dhall (for dhall-format-tasty-compare)
# dhall-hpack-cabal (for hpack-dhall-tasty-compare)
tasty-compare.cabal is up-to-date
# dhall (for dhall-format-app-serve)
# dhall-hpack-cabal (for hpack-dhall-app-serve)
app-serve.cabal is up-to-date
# dhall (for dhall-format-zone)
# dhall-hpack-cabal (for hpack-dhall-zone)
flight-zone.cabal is up-to-date
# dhall (for dhall-format-units)
# dhall-hpack-cabal (for hpack-dhall-units)
flight-units.cabal is up-to-date
# dhall (for dhall-format-track)
# dhall-hpack-cabal (for hpack-dhall-track)
flight-track.cabal is up-to-date
# dhall (for dhall-format-time)
# dhall-hpack-cabal (for hpack-dhall-time)
flight-time.cabal is up-to-date
# dhall (for dhall-format-task)
# dhall-hpack-cabal (for hpack-dhall-task)
flight-task.cabal is up-to-date
# dhall (for dhall-format-span)
# dhall-hpack-cabal (for hpack-dhall-span)
flight-span.cabal is up-to-date
# dhall (for dhall-format-siggy-chardust)
# dhall-hpack-cabal (for hpack-dhall-siggy-chardust)
siggy-chardust.cabal is up-to-date
# dhall (for dhall-format-scribe)
# dhall-hpack-cabal (for hpack-dhall-scribe)
flight-scribe.cabal is up-to-date
# dhall (for dhall-format-route)
# dhall-hpack-cabal (for hpack-dhall-route)
flight-route.cabal is up-to-date
# dhall (for dhall-format-mask)
# dhall-hpack-cabal (for hpack-dhall-mask)
flight-mask.cabal is up-to-date
# dhall (for dhall-format-lookup)
# dhall-hpack-cabal (for hpack-dhall-lookup)
flight-lookup.cabal is up-to-date
# dhall (for dhall-format-latlng)
# dhall-hpack-cabal (for hpack-dhall-latlng)
flight-latlng.cabal is up-to-date
# dhall (for dhall-format-kml)
# dhall-hpack-cabal (for hpack-dhall-kml)
flight-kml.cabal is up-to-date
# dhall (for dhall-format-igc)
# dhall-hpack-cabal (for hpack-dhall-igc)
flight-igc.cabal is up-to-date
# dhall (for dhall-format-gap)
# dhall-hpack-cabal (for hpack-dhall-gap)
flight-gap.cabal is up-to-date
# dhall (for dhall-format-fsdb)
# dhall-hpack-cabal (for hpack-dhall-fsdb)
flight-fsdb.cabal is up-to-date
# dhall (for dhall-format-flare-timing)
# dhall-hpack-cabal (for hpack-dhall-flare-timing)
flare-timing.cabal is up-to-date
# dhall (for dhall-format-earth)
# dhall-hpack-cabal (for hpack-dhall-earth)
flight-earth.cabal is up-to-date
# dhall (for dhall-format-comp)
# dhall-hpack-cabal (for hpack-dhall-comp)
flight-comp.cabal is up-to-date
# dhall (for dhall-format-clip)
# dhall-hpack-cabal (for hpack-dhall-clip)
flight-clip.cabal is up-to-date
# dhall (for dhall-format-cmd)
# dhall-hpack-cabal (for hpack-dhall-cmd)
flight-cmd.cabal is up-to-date
# dhall (for dhall-format-build)
# dhall-hpack-cabal (for hpack-dhall-build)
build-flare-timing.cabal is up-to-date
# dhall (for dhall-format-detour-via-uom)
# dhall-hpack-cabal (for hpack-dhall-detour-via-uom)
detour-via-uom.cabal is up-to-date
# dhall (for dhall-format-detour-via-sci)
# dhall-hpack-cabal (for hpack-dhall-detour-via-sci)
detour-via-sci.cabal is up-to-date
Build completed in 0:02m
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
# stack (for stack-doctest-flight-track)
flight-track-0.1.0: test (suite: doctest)

Examples: 19  Tried: 19  Errors: 0  Failures: 0

flight-track-0.1.0: Test suite doctest passed
# stack (for stack-doctest-flight-igc)
flight-igc-2.0.0: test (suite: doctest)

Examples: 87  Tried: 87  Errors: 0  Failures: 0

flight-igc-2.0.0: Test suite doctest passed
# stack (for stack-doctest-flight-kml)
flight-kml-1.1.0: test (suite: doctest)

Examples: 57  Tried: 57  Errors: 0  Failures: 0

flight-kml-1.1.0: Test suite doctest passed
# stack (for stack-doctest-flight-comp)
flight-comp-0.1.0: test (suite: doctest)

Examples: 5  Tried: 5  Errors: 0  Failures: 0

flight-comp-0.1.0: Test suite doctest passed
# stack (for stack-doctest-flight-clip)
flight-clip-1.1.0: test (suite: doctest)

Examples: 12  Tried: 12  Errors: 0  Failures: 0

flight-clip-1.1.0: Test suite doctest passed
# stack (for stack-doctest-siggy-chardust)
siggy-chardust-1.0.0: test (suite: doctest)

Examples: 35  Tried: 35  Errors: 0  Failures: 0

siggy-chardust-1.0.0: Test suite doctest passed
# stack (for stack-doctest-detour-via-uom)
...
detour-via-uom-1.0.1: test (suite: doctest)

Examples: 30  Tried: 30  Errors: 0  Failures: 0

detour-via-uom-1.0.1: Test suite doctest passed
# stack (for stack-doctest-detour-via-sci)
detour-via-sci-1.0.1: test (suite: doctest)

Examples: 50  Tried: 50  Errors: 0  Failures: 0

detour-via-sci-1.0.1: Test suite doctest passed
Build completed in 8:27m
```

The `doctest` targets can be run individually too;

```
> ./stack-shake-build.sh stack-doctest-flight-kml
...
# stack (for stack-doctest-flight-kml)
flight-kml-1.1.0: test (suite: doctest)

Examples: 57  Tried: 57  Errors: 0  Failures: 0

flight-kml-1.1.0: Test suite doctest passed
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
* [`flight-clip`](clip)
* [`flight-comp`](comp)
* [`flight-gap`](gap)
* [`flight-lookup`](lookup)
* [`flight-mask`](mask)
* [`flight-task`](task)
* [`flight-time`](time)
* [`flight-route`](route)

To do with files read from and written to during scoring;
* [`flight-scribe`](scribe)
* [`flight-track`](track)

Testing;
* [`tasty-compare`](tasty-compare)

The command line programs for scoring are in `./flare-timing`.
