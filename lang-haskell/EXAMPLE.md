# Worked Example

This example covers both scoring and displaying scores.  We're going to be
working with the Forbes Flatlands comp of 2014.

## Setup

For both scoring and display we'll get the source and checkout a release.

```
> git clone https://github.com/BlockScope/flare-timing.git --recursive
> cd flare-timing
> stack install
...
Copied executables to /Users/.../.local/bin:
- align-time
- area-step
- build-flare-timing
- comp-py
- comp-serve
- cross-zone
- discard-further
- extract-input
- fs-arrival
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
- unpack-track
```

## Scoring

Scoring is time consuming and has already been done and commited to the repo
with the scores for this comp so this step can be skipped if you just want to
see the display.

Now that we've installed the command line apps needed for scoring we can invoke
a script that calls those commands in order. The last command starts the
server.

```
> git clone https://github.com/FlareTiming/comps-forbes-flatlands.git
> cd comps-forbes-flatlands/2014
.../2014> ./ft-score.sh
```

## Display

In one terminal, start the server:

```
.../2014> comp-serve --file=forbes2014
Reading task length from 'forbes2014.task-length.yaml'
Reading competition & pilots DNF from 'forbes2014.comp-input.yaml'
Reading flying time range from 'forbes2014.cross-zone.yaml'
Reading zone tags from 'forbes2014.tag-zone.yaml'
Reading scored section from 'forbes2014.peg-frame.yaml'
Reading arrivals from 'forbes2014.mask-arrival.yaml'
Reading effort from 'forbes2014.mask-effort.yaml'
Reading leading area from 'forbes2014.lead-area.yaml'
Reading leading from 'forbes2014.mask-lead.yaml'
Reading reach from 'forbes2014.mask-reach.yaml'
Reading speed from 'forbes2014.mask-speed.yaml'
Reading bonus reach from 'forbes2014.bonus-reach.yaml'
Reading land outs from 'forbes2014.land-out.yaml'
Reading scores from 'forbes2014.gap-point.yaml'
Reading expected or normative arrivals from 'forbes2014.norm-arrival.yaml'
Reading expected or normative land outs from 'forbes2014.norm-land-out.yaml'
Reading expected or normative optimal routes from 'forbes2014.norm-route.yaml'
Reading expected or normative scores from 'forbes2014.norm-score.yaml'
listening on port 3000
```

First we need to make a small edit to `app-view/comp-view/FlareTiming/Comms.hs`
so that we're picking up the served data rather than picking up the data from
`.json` files as we do when publishing a comp as a static site.

```diff
mapUri :: T.Text -> T.Text
-- mapUri s = "/json" <> s <> ".json"
++ mapUri s = "http://localhost:3000" <> s
```

In another terminal, build and start the client dev server:

```
.../lang-haskell> reflex-platform/try-reflex
...
You are now in a shell with access to the Reflex functional reactive programming engine.
...
[nix-shell:~/.../lang-haskell]$ ./stack-shake-build.sh view-start-ghcjs
...
- build-flare-timing
# pack app.html
ghcjs
            -DGHCJS_BROWSER
            -Wall
            -outputdir ../../__www-build-ghcjs/app.jsout
            -o ../../__www-build-ghcjs/app.jsexe
            App.hs
...
[95 of 95] Compiling Main
Linking ../../__www-build-ghcjs/app.jsexe
# ghcjs (for __www-build-ghcjs/app.jsexe/all.js)
...
# copy all.js
# install node_modules
...
# yarn (for __www-dist-ghcjs/task-view/app.html)
yarn run v1.22.4
$ webpack --config=webpack-ghcjs.config.js
...
# yarn (for view-start-ghcjs)
yarn run v1.22.4
$ webpack-dev-server --config=webpack-ghcjs.config.js
ℹ ｢wds｣: Project is running at http://localhost:9000/
...
```

Navigating to http://localhost:9000/app.html should show a page listing the
tasks of the **Australian National Hang Gliding Championships 2014**.

When editing frontend code, sometimes you may need to manually clean to force
the build to pickup edits:

```
[nix-shell:~/.../lang-haskell]$ ./stack-shake-build.sh clean-www
[nix-shell:~/.../lang-haskell]$ ./stack-shake-build.sh view-start-ghcjs
```
