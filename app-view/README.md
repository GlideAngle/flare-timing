# app-view

This is the frontend of flare-timing, the web app. Like most other apps here,
it too is written in Haskell but compiled with
[ghcjs](https://github.com/ghcjs/ghcjs), targeting javascript.
A [reflex-frp](https://reflex-frp.org/) app, it shows the tasks, maps and scores
of a competition as for [QuestAir Open 2016](http://2016-quest.flaretiming.com).

Start [`app-serve`](../app-serve), the backend, first. From the root directory
of this repository, host the frontend [locally](http://localhost:9000/app.html)
with:

```
> reflex-platform/try-reflex
[nix-shell:flare-timing]$ ./stack-shake-build.sh view-start-ghcjs

ghcjs
            -DGHCJS_BROWSER
            -XConstraintKinds -XDataKinds -XDeriveFunctor
            -XDeriveGeneric-XDeriveAnyClass -XDerivingStrategies
            -XDisambiguateRecordFields -XDuplicateRecordFields
            -XFlexibleContexts -XFlexibleInstances -XGeneralizedNewtypeDeriving
            -XGADTs -XKindSignatures -XLambdaCase -XMonoLocalBinds
            -XMultiParamTypeClasses -XMultiWayIf -XNamedFieldPuns
            -XOverloadedStrings -XPackageImports -XParallelListComp
            -XPartialTypeSignatures -XPatternSynonyms -XQuasiQuotes
            -XRankNTypes -XRecursiveDo -XRecordWildCards -XScopedTypeVariables
            -XStandaloneDeriving -XTemplateHaskell -XTypeApplications
            -XTypeFamilies -XTypeOperators -XTypeSynonymInstances
            -XTupleSections -XUndecidableInstances
            -Wall
            -outputdir ../../__www-build-ghcjs/app.jsout
            -o ../../__www-build-ghcjs/app.jsexe
            App.hs
# ghcjs (for __www-build-ghcjs/app.jsexe/all.js)
[ 1 of 87] Compiling Data.Ratio.Rounding
[ 2 of 87] Compiling FlareTiming.Comp.Tab
[ 3 of 87] Compiling FlareTiming.Events
[ 4 of 87] Compiling FlareTiming.Footer
[ 5 of 87] Compiling FlareTiming.Katex
[ 6 of 87] Compiling FlareTiming.Nav.TabBasis
[ 7 of 87] Compiling FlareTiming.Nav.TabPlot
[ 8 of 87] Compiling FlareTiming.Nav.TabScore
[ 9 of 87] Compiling FlareTiming.Nav.TabTask
[10 of 87] Compiling FlareTiming.Plot.Foreign
[11 of 87] Compiling FlareTiming.Plot.Effort.Plot
[12 of 87] Compiling FlareTiming.Plot.Arrival.Plot
[13 of 87] Compiling FlareTiming.Plot.Lead.Plot
[14 of 87] Compiling FlareTiming.Plot.Reach.Plot
[15 of 87] Compiling FlareTiming.Plot.Time.Plot
[16 of 87] Compiling FlareTiming.Statistics
[17 of 87] Compiling FlareTiming.Task.Validity.Widget
[18 of 87] Compiling WireTypes.Fraction
[19 of 87] Compiling WireTypes.Arrival
[20 of 87] Compiling WireTypes.Lead
[21 of 87] Compiling WireTypes.Speed
[22 of 87] Compiling WireTypes.Point
[23 of 87] Compiling WireTypes.Pilot
[24 of 87] Compiling WireTypes.Effort
[25 of 87] Compiling WireTypes.Validity
[26 of 87] Compiling WireTypes.ZoneKind
[27 of 87] Compiling WireTypes.Zone
[28 of 87] Compiling WireTypes.Route
[29 of 87] Compiling WireTypes.Cross
[30 of 87] Compiling FlareTiming.Earth
[31 of 87] Compiling FlareTiming.Map.Leaflet
[32 of 87] Compiling WireTypes.Comp
[33 of 87] Compiling FlareTiming.Time
[34 of 87] Compiling WireTypes.ValidityWorking
[35 of 87] Compiling WireTypes.Reach 
[36 of 87] Compiling FlareTiming.Task.Validity.Time
[37 of 87] Compiling FlareTiming.Task.Validity.Task
[38 of 87] Compiling FlareTiming.Task.Validity.Stop.StdDev
[39 of 87] Compiling FlareTiming.Task.Validity.Stop.Mean
[40 of 87] Compiling FlareTiming.Task.Validity.Stop.Max
[41 of 87] Compiling FlareTiming.Task.Validity.Stop.Counts
[42 of 87] Compiling FlareTiming.Task.Validity.Launch
[43 of 87] Compiling FlareTiming.Task.Validity.Distance
[44 of 87] Compiling FlareTiming.Plot.Valid.Plot
[45 of 87] Compiling FlareTiming.Plot.Valid.View
[46 of 87] Compiling FlareTiming.Plot.Valid
[47 of 87] Compiling FlareTiming.Task.Score.Show
[48 of 87] Compiling FlareTiming.Plot.Weight.Working
[49 of 87] Compiling FlareTiming.Plot.Weight.Plot
[50 of 87] Compiling FlareTiming.Plot.Weight.View
[51 of 87] Compiling FlareTiming.Plot.Weight
[52 of 87] Compiling FlareTiming.Pilot
[53 of 87] Compiling FlareTiming.Task.Validity.Stop
[54 of 87] Compiling FlareTiming.Task.Validity
[55 of 87] Compiling FlareTiming.Task.Score.Time
[56 of 87] Compiling FlareTiming.Task.Score.Split
[57 of 87] Compiling FlareTiming.Task.Score.Speed
[58 of 87] Compiling FlareTiming.Task.Score.Reach
[59 of 87] Compiling FlareTiming.Task.Score.Over
[60 of 87] Compiling FlareTiming.Task.Score.Effort
[61 of 87] Compiling FlareTiming.Task.Score.Arrive
[62 of 87] Compiling FlareTiming.Plot.Time.View
[63 of 87] Compiling FlareTiming.Plot.Time
[64 of 87] Compiling FlareTiming.Plot.Reach.View
[65 of 87] Compiling FlareTiming.Plot.Reach
[66 of 87] Compiling FlareTiming.Plot.Lead.View
[67 of 87] Compiling FlareTiming.Plot.Lead
[68 of 87] Compiling FlareTiming.Plot.Effort.View
[69 of 87] Compiling FlareTiming.Plot.Effort
[70 of 87] Compiling FlareTiming.Plot.Arrival.View
[71 of 87] Compiling FlareTiming.Plot.Arrival
[72 of 87] Compiling FlareTiming.Comp.Header
[73 of 87] Compiling FlareTiming.Comms
[74 of 87] Compiling FlareTiming.Task.Geo
[75 of 87] Compiling FlareTiming.Task.Absent
[76 of 87] Compiling FlareTiming.Comp.Pilot
[77 of 87] Compiling FlareTiming.Breadcrumb
[78 of 87] Compiling FlareTiming.Turnpoint
[79 of 87] Compiling FlareTiming.Comp.Tasks
[80 of 87] Compiling FlareTiming.Task.Turnpoints
[81 of 87] Compiling FlareTiming.Map.View
[82 of 87] Compiling FlareTiming.Task.Detail
[83 of 87] Compiling FlareTiming.Comp.Settings
[84 of 87] Compiling FlareTiming.Comp.Detail
[85 of 87] Compiling FlareTiming.Task
[86 of 87] Compiling FlareTiming.View
[87 of 87] Compiling Main
Linking ../../__www-build-ghcjs/app.jsexe
# copy all.js
# pack app.html
# yarn (for __www-dist-ghcjs/task-view/app.html)
yarn run v1.17.3
$ webpack-dev-server --config=webpack-ghcjs.config.js
ℹ ｢wds｣: Project is running at http://localhost:9000/
ℹ ｢wds｣: webpack output is served from /
ℹ ｢wds｣: Content not from webpack is served from /Users/_/flare-timing/__www-dist-ghcjs/task-view
ℹ ｢wdm｣: Compiled with warnings.
⚠ ｢wdm｣: Hash: 39196bd52bb6f363bfff
Version: webpack 4.33.0
Time: 4644ms
Built at: 2019-07-18 12:32:29
                                 Asset      Size  Chunks                    Chunk Names
  674f50d287a8c48dc19ba404d20fe713.eot   162 KiB          [emitted]
  912ec66d7572ff821749319396470bde.svg   434 KiB          [emitted]  [big]
af7ae505a9eed503f8b8e6982036873e.woff2  75.4 KiB          [emitted]
                              app.html  1.57 KiB          [emitted]
                                app.js   145 KiB       0  [emitted]         app
                            app.js.map   470 KiB       0  [emitted]         app
  b06871f281fee6b241d60582ae9369b9.ttf   162 KiB          [emitted]
 fee66e712a8a08eef5805a46892932ad.woff  95.7 KiB          [emitted]
                            styles.css   673 KiB       0  [emitted]  [big]  app
                        styles.css.map  87 bytes       0  [emitted]         app
Entrypoint app [big] = app.js styles.css app.js.map styles.css.map
 [0] (webpack)-dev-server/lib/clients/SockJSClient.js 598 bytes {0} [built]
 [3] (webpack)-dev-server/client/utils/log.js 964 bytes {0} [built]
 [4] ./node_modules/querystring-es3/index.js 127 bytes {0} [built]
 [5] multi (webpack)-dev-server/client?http://localhost:9000 ./app.js 40 bytes {0} [built]
 [6] (webpack)-dev-server/client?http://localhost:9000 4.29 KiB {0} [built]
 [7] ./node_modules/strip-ansi/index.js 161 bytes {0} [built]
 [8] ./node_modules/ansi-regex/index.js 135 bytes {0} [built]
 [9] (webpack)-dev-server/client/socket.js 1.04 KiB {0} [built]
[12] (webpack)-dev-server/client/overlay.js 3.59 KiB {0} [built]
[18] (webpack)-dev-server/client/utils/sendMessage.js 402 bytes {0} [built]
[19] (webpack)-dev-server/client/utils/reloadApp.js 1.63 KiB {0} [built]
[22] (webpack)-dev-server/client/utils/createSocketUrl.js 2.77 KiB {0} [built]
[30] (webpack)/hot sync nonrecursive ^\.\/log$ 170 bytes {0} [built]
[32] ./app.js 62 bytes {0} [built]
[33] ./app.html 54 bytes {0} [built]
    + 28 hidden modules
```
