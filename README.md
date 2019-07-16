# Flare Timing

[![Build Status](https://travis-ci.org/BlockScope/flare-timing.svg)](https://travis-ci.org/BlockScope/flare-timing)

**Flare Timing** is a reference implementation of [GAP](GAP.md). It consists of
a series of small command line console apps that write down the working from
each step in calculating pilot scores. As such data entry and scoring is
headless but we do include a web app that can be hosted locally for visual
checks and comparisons with expected or official results. It is possible to
publish the data alongside this web app standalone as done at [flaretiming, the
web site](https://flaretiming.com).

## Usage

Let's get the inputs and outputs from FS, do the scoring and host the comp
locally. [Building](BUILDING.md) and [testing](TESTING.md) have their own
guides.

Start by preparing the FS comp file, the `*.fsdb`. This will often contain
sensitive personal information such as birthdays, phone numbers and notes that
we'll want to avoid publishing. We'll also want some output data for making
comparisons between flare-timing and FS.

1. Clean out the sensitive stuff and trim away data we don't need with
[`fs-filter`](flare-timing/prod-apps/fs-filter).  
2. Grab the optimal route around the tasks found by FS with
[`fs-route`](flare-timing/prod-apps/fs-route).  
3. Grab the landouts from FS with
[`fs-effort`](flare-timing/prod-apps/fs-effort).  
3. Grab the scores from FS with
[`fs-score`](flare-timing/prod-apps/fs-score).  

That's the `*.fsdb` file done with. From here on, flare-timing deals with the
trimmed XML, the `*.trim-fsdb.xml`. If we have the track logs we can score the
tasks:

1. Extract the inputs with
[`extract-input`](flare-timing/prod-apps/extract-input).  
2. Trace the shortest path to fly a task with
[`task-length`](flare-timing/prod-apps/task-length).  
3. Find pairs of fixes crossing over zones with
[`cross-zone`](flare-timing/prod-apps/cross-zone).  
4. Interpolate between crossing fixes for the time and place where a track tags
a zone with [`tag-zone`](flare-timing/prod-apps/tag-zone).  
5. Peg the timing window to a reference frame with
[`peg-frame`](flare-timing/prod-apps/peg-frame).  
6. Index fixes from the time of first crossing with
[`align-time`](flare-timing/prod-apps/align-time).  
7. Discard fixes that get further from goal and note leading area with
[`discard-further`](flare-timing/prod-apps/discard-further).  
8. Mask a task over its tracklogs with
[`mask-track`](flare-timing/prod-apps/mask-track).  
9. Group and count land outs with
[`land-out`](flare-timing/prod-apps/land-out).  
10. Score the competition with [`gap-point`](flare-timing/prod-apps/gap-point).  

To host the backend server for the comp data locally:

1. Unpack the `*.igc` or `*.kml` tracklogs with
[`unpack-track`](flare-timing/prod-apps/unpack-track).  
2. Start the server with
[`comp-serve`](flare-timing/app-serve).  

To host the frontend web app for the comp locally:

1. Open a try-reflex shell with:
    `> reflex-platform/try-reflex`
2. Build the frontend and start its webpack dev server with:
    `> ./stack-shake-build.sh view-start-ghcjs`
3. Open a browser at the hosted URL, usually http://localhost:9000/app.html.


Documentation is available online at
[flare-timing.readthedocs.io](http://flare-timing.readthedocs.io/) and there's
a [worked example](EXAMPLE.md) too.

## License

```
Copyright © Phil de Joux 2017-2019
Copyright © Block Scope Limited 2017-2019
```

This software is subject to the terms of the Mozilla Public License, v2.0. If
a copy of the MPL was not distributed with this file, you can obtain one at
http://mozilla.org/MPL/2.0/.
