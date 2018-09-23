# Building

I'm building with [try-reflex](https://github.com/reflex-frp/reflex-platform/blob/develop/try-reflex) and have added
[ghcjs-ffiqq](https://github.com/ghcjs/ghcjs-ffiqq) to
[packages.nix](https://github.com/reflex-frp/reflex-platform/blob/develop/packages.nix).

With the reflex-platform repo as a peer to the flare-timing repo ...

Serve a competition FS database file ...

```
$ ./__shake-build/flight-fsdb-serve --file forbes.fsdb
Drive {file = "forbes.fsdb"}
ServeOptions {file = "forbes.fsdb"}
forbes.fsdb
listening on port 3000
```

Build and host the client locally ...

```
$ ../reflex-platform/try-reflex
If you have any trouble with this script, please submit an issue at
https://github.com/reflex-frp/reflex-platform/issues
Entering the reflex sandbox...

You are now in a shell with access to the Reflex functional reactive programming engine.

$ ./build.sh view-www
...
#phony view-www
#compile all.js
# ghcjs (for view/app.jsexe/all.js)
[1 of 9] Compiling FlareTiming.NavBar ( FlareTiming/NavBar.hs, app.jsout/FlareTiming/NavBar.js_o )
[2 of 9] Compiling FlareTiming.Map.Leaflet ( FlareTiming/Map/Leaflet.hs, app.jsout/FlareTiming/Map/Leaflet.js_o )
[3 of 9] Compiling FlareTiming.Footer ( FlareTiming/Footer.hs, app.jsout/FlareTiming/Footer.js_o )
[4 of 9] Compiling Data.Flight.Types ( Data/Flight/Types.hs, app.jsout/Data/Flight/Types.js_o )
[5 of 9] Compiling FlareTiming.Map  ( FlareTiming/Map.hs, app.jsout/FlareTiming/Map.js_o )
[6 of 9] Compiling FlareTiming.Turnpoint ( FlareTiming/Turnpoint.hs, app.jsout/FlareTiming/Turnpoint.js_o )
[7 of 9] Compiling FlareTiming.Task ( FlareTiming/Task.hs, app.jsout/FlareTiming/Task.js_o )
[8 of 9] Compiling FlareTiming.View ( FlareTiming/View.hs, app.jsout/FlareTiming/View.js_o )
[9 of 9] Compiling Main             ( App.hs, app.jsout/Main.js_o )
Linking App.jsexe (Data.Flight.Types,FlareTiming.Footer,FlareTiming.Map,FlareTiming.Map.Leaflet,
FlareTiming.NavBar,FlareTiming.Task,FlareTiming.Turnpoint,FlareTiming.View,Main)
#copy all.js
Build completed in 0:11m

$ ./build.sh view-start

# yarn (for view-start)
yarn run v0.24.6
$ webpack-dev-server
Project is running at http://localhost:9000/
webpack output is served from /
Content not from webpack is served from /Users/.../flare-timing/__www/task-view
(node:7083) DeprecationWarning: Chunk.modules is deprecated.
Use Chunk.getNumberOfModules/mapModules/forEachModule/containsModule instead.
Hash: 77a6aa4369a7982c9eb5
Version: webpack 3.0.0
Time: 2198ms
                                 Asset       Size  Chunks                    Chunk Names
  674f50d287a8c48dc19ba404d20fe713.eot     166 kB          [emitted]
af7ae505a9eed503f8b8e6982036873e.woff2    77.2 kB          [emitted]
 fee66e712a8a08eef5805a46892932ad.woff      98 kB          [emitted]
  b06871f281fee6b241d60582ae9369b9.ttf     166 kB          [emitted]
  912ec66d7572ff821749319396470bde.svg     444 kB          [emitted]  [big]
                              app.html  793 bytes          [emitted]
                                app.js     316 kB       0  [emitted]  [big]  app
                            styles.css     164 kB       0  [emitted]         app
                            app.js.map     377 kB       0  [emitted]         app
                        styles.css.map   87 bytes       0  [emitted]         app
  [35] multi (webpack)-dev-server/client?http://localhost:9000 ./app.js 40 bytes {0} [built]
  [36] (webpack)-dev-server/client?http://localhost:9000 5.78 kB {0} [built]
  [37] ./node_modules/url/url.js 23.3 kB {0} [built]
  [43] ./node_modules/strip-ansi/index.js 161 bytes {0} [built]
  [45] (webpack)-dev-server/client/socket.js 897 bytes {0} [built]
  [77] (webpack)-dev-server/client/overlay.js 3.73 kB {0} [built]
  [78] ./node_modules/ansi-html/index.js 4.26 kB {0} [built]
  [82] (webpack)/hot/emitter.js 77 bytes {0} [built]
  [83] ./node_modules/events/events.js 8.33 kB {0} [built]
  [84] ./app.js 62 bytes {0} [built]
  [85] ./app.html 54 bytes {0} [built]
  [86] ./site.sass 41 bytes {0} [built]
  [88] ./node_modules/font-awesome/fonts/fontawesome-webfont.eot?v=4.7.0 82 bytes [built]
  [89] ./node_modules/font-awesome/fonts/fontawesome-webfont.eot 82 bytes [built]
  [92] ./node_modules/font-awesome/fonts/fontawesome-webfont.ttf?v=4.7.0 82 bytes [built]
    + 79 hidden modules
Child extract-text-webpack-plugin:
       [0] ./node_modules/css-loader!./node_modules/sass-loader/lib/loader.js!./site.sass 173 kB {0} [built]
       [1] ./node_modules/css-loader/lib/css-base.js 2.26 kB {0} [built]
       [2] ./node_modules/font-awesome/fonts/fontawesome-webfont.eot?v=4.7.0 82 bytes {0} [built]
       [3] ./node_modules/font-awesome/fonts/fontawesome-webfont.eot 82 bytes {0} [built]
       [4] ./node_modules/font-awesome/fonts/fontawesome-webfont.woff2?v=4.7.0 84 bytes {0} [built]
       [5] ./node_modules/font-awesome/fonts/fontawesome-webfont.woff?v=4.7.0 83 bytes {0} [built]
       [6] ./node_modules/font-awesome/fonts/fontawesome-webfont.ttf?v=4.7.0 82 bytes {0} [built]
       [7] ./node_modules/font-awesome/fonts/fontawesome-webfont.svg?v=4.7.0 82 bytes {0} [built]
webpack: Compiled successfully.
```
