# Flare Timing's View App

This internally named `app-view` project is a web app written in
[ghcjs](https://github.com/ghcjs/ghcjs) and
[reflex-frp](https://reflex-frp.org/) to show the task, the map and the scores
of a competition.

Fire up the [`app-serve`](flare-timing/app-serve) internal project first. We'll
need that to supply data for the viewer. The `FlareTiming.Comms` module fetches
this data.

To host it locally with webpack's [dev
server](https://webpack.js.org/configuration/dev-server/), follow the following
command sequence starting from the root directory of this repository:

```
> reflex-platform/try-reflex
[nix-shell:flare-timing]$ ./stack-shake-build.sh view-start-ghcjs

Copied executables to .../__shake-build:
- build-flare-timing

ghcjs
            -DGHCJS_BROWSER
            -XConstraintKinds ... -XUndecidableInstances
            -Wall
            -outputdir ../../__www-build-ghcjs/app.jsout
            -o ../../__www-build-ghcjs/app.jsexe
            App.hs
# ghcjs (for __www-build-ghcjs/app.jsexe/all.js)
[ 1 of 33] Compiling FlareTiming.Comp.Tab
[ 2 of 33] Compiling FlareTiming.Events
[ 3 of 33] Compiling FlareTiming.Footer
[ 4 of 33] Compiling FlareTiming.Task.Tab
[ 5 of 33] Compiling WireTypes.Pilot
[ 6 of 33] Compiling FlareTiming.Pilot
[ 7 of 33] Compiling FlareTiming.Map.Leaflet
[ 8 of 33] Compiling WireTypes.Point
[ 9 of 33] Compiling WireTypes.Validity
[10 of 33] Compiling WireTypes.ZoneKind
[11 of 33] Compiling WireTypes.Zone
[12 of 33] Compiling WireTypes.Route
[13 of 33] Compiling WireTypes.Comp
[14 of 33] Compiling FlareTiming.Time
[15 of 33] Compiling WireTypes.ValidityWorking
[16 of 33] Compiling FlareTiming.Task.Validity
[17 of 33] Compiling FlareTiming.Task.Score
[18 of 33] Compiling FlareTiming.Comp.Header
[19 of 33] Compiling FlareTiming.Comms
[20 of 33] Compiling FlareTiming.Task.Geo
[21 of 33] Compiling FlareTiming.Task.Absent
[22 of 33] Compiling FlareTiming.Comp.Pilot
[23 of 33] Compiling FlareTiming.Breadcrumb
[24 of 33] Compiling FlareTiming.Turnpoint
[25 of 33] Compiling FlareTiming.Comp.Tasks
[26 of 33] Compiling FlareTiming.Task.Turnpoints
[27 of 33] Compiling FlareTiming.Map.View
[28 of 33] Compiling FlareTiming.Task.Detail
[29 of 33] Compiling FlareTiming.Comp.Settings
[30 of 33] Compiling FlareTiming.Comp.Detail
[31 of 33] Compiling FlareTiming.Task
[32 of 33] Compiling FlareTiming.View
[33 of 33] Compiling Main
Linking ../../__www-build-ghcjs/app.jsexe
# copy all.js
# pack app.html
# yarn (for __www-dist-ghcjs/task-view/app.html)
yarn run v1.12.3
$ webpack --config=webpack-ghcjs.config.js
Hash: c5836405997231ab1664
Version: webpack 3.12.0
Time: 2161ms
                                 Asset      Size  Chunks                    Chunk Names
  674f50d287a8c48dc19ba404d20fe713.eot    166 kB          [emitted]
af7ae505a9eed503f8b8e6982036873e.woff2   77.2 kB          [emitted]
 fee66e712a8a08eef5805a46892932ad.woff     98 kB          [emitted]
  b06871f281fee6b241d60582ae9369b9.ttf    166 kB          [emitted]
  912ec66d7572ff821749319396470bde.svg    444 kB          [emitted]  [big]
                              app.html   1.35 kB          [emitted]
                                app.js   2.82 kB       0  [emitted]         app
                            styles.css    305 kB       0  [emitted]  [big]  app
                            app.js.map   3.09 kB       0  [emitted]         app
                        styles.css.map  87 bytes       0  [emitted]         app
   [0] ./app.js 62 bytes {0} [built]
   [1] ./app.html 54 bytes {0} [built]
   [2] ./site.sass 41 bytes {0} [built]
    + 8 hidden modules
Child extract-text-webpack-plugin:
     5 assets
       [0] ./node_modules/css-loader!./node_modules/sass-loader/lib/loader.js!./site.sass 317 kB {0} [built]
        + 8 hidden modules
✨  Done in 2.86s.
# yarn (for view-start-ghcjs)
yarn run v1.12.3
$ webpack-dev-server --config=webpack-ghcjs.config.js
Project is running at http://localhost:9000/
webpack output is served from /
Content not from webpack is served from .../__www-dist-ghcjs/task-view
Hash: c55bab2449a0d0bde4c0
Version: webpack 3.12.0
Time: 2196ms
                                 Asset      Size  Chunks                    Chunk Names
  674f50d287a8c48dc19ba404d20fe713.eot    166 kB          [emitted]
af7ae505a9eed503f8b8e6982036873e.woff2   77.2 kB          [emitted]
 fee66e712a8a08eef5805a46892932ad.woff     98 kB          [emitted]
  b06871f281fee6b241d60582ae9369b9.ttf    166 kB          [emitted]
  912ec66d7572ff821749319396470bde.svg    444 kB          [emitted]  [big]
                              app.html   1.35 kB          [emitted]
                                app.js    325 kB       0  [emitted]  [big]  app
                            styles.css    305 kB       0  [emitted]  [big]  app
                            app.js.map    386 kB       0  [emitted]         app
                        styles.css.map  87 bytes       0  [emitted]         app
   [2] multi (webpack)-dev-server/client?http://localhost:9000 ./app.js 40 bytes {0} [built]
   [3] (webpack)-dev-server/client?http://localhost:9000 7.93 kB {0} [built]
   [4] ./node_modules/url/url.js 23.3 kB {0} [built]
   [5] ./node_modules/punycode/punycode.js 14.7 kB {0} [built]
  [11] ./node_modules/strip-ansi/index.js 161 bytes {0} [built]
  [13] ./node_modules/loglevel/lib/loglevel.js 7.86 kB {0} [built]
  [14] (webpack)-dev-server/client/socket.js 1.08 kB {0} [built]
  [16] (webpack)-dev-server/client/overlay.js 3.67 kB {0} [built]
  [18] ./node_modules/html-entities/index.js 231 bytes {0} [built]
  [21] (webpack)/hot nonrecursive ^\.\/log$ 170 bytes {0} [built]
  [23] (webpack)/hot/emitter.js 77 bytes {0} [built]
  [25] ./app.js 62 bytes {0} [built]
  [26] ./app.html 54 bytes {0} [built]
  [27] ./site.sass 41 bytes {0} [built]
  [34] ./node_modules/font-awesome/fonts/fontawesome-webfont.ttf?v=4.7.0 82 bytes [built]
    + 21 hidden modules
Child extract-text-webpack-plugin:
     5 assets
       [0] ./node_modules/css-loader!./node_modules/sass-loader/lib/loader.js!./site.sass 317 kB {0} [built]
       [1] ./node_modules/css-loader/lib/url/escape.js 448 bytes {0} [built]
       [2] ./node_modules/css-loader/lib/css-base.js 2.26 kB {0} [built]
       [3] ./node_modules/font-awesome/fonts/fontawesome-webfont.eot?v=4.7.0 82 bytes {0} [built]
       [4] ./node_modules/font-awesome/fonts/fontawesome-webfont.eot 82 bytes {0} [built]
       [5] ./node_modules/font-awesome/fonts/fontawesome-webfont.woff2?v=4.7.0 84 bytes {0} [built]
       [6] ./node_modules/font-awesome/fonts/fontawesome-webfont.woff?v=4.7.0 83 bytes {0} [built]
       [7] ./node_modules/font-awesome/fonts/fontawesome-webfont.ttf?v=4.7.0 82 bytes {0} [built]
       [8] ./node_modules/font-awesome/fonts/fontawesome-webfont.svg?v=4.7.0 82 bytes {0} [built]
webpack: Compiled successfully.
```
