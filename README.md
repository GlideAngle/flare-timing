# Flare Timing

[![Build Status](https://travis-ci.org/blockscope/flare-timing.svg)](https://travis-ci.org/blockscope/flare-timing)

**Flare Timing** will show a pilot's competition score in detail but is
currently a work in progress.

The official scoring program for hang gliding and paragliding competitions is
[FS](http://fs.fai.org/). Its principal output for a competition task is an
html table of pilot scores. In addition to the day's scores, there are columns
in this table for the time on course, the distance flown and for the breakdown
of the scored points among distance points, lead points, time points and
arrival points.

Looking for examples? There is a good archive of past Forbes Flatlands
[competition results](http://www.forbesflatlands.com/results/past-results), for
instance results for [task
1](https://www.forbesflatlands.com/results-show?id_results=7&db=results2013&class=results_open)
of the 19th World Hang Gliding Champs.

Commission Internationale de Vol Libre (CIVL - Hang Gliding and Paragliding
Commission) is an Air Sport Commission (ASC) of the Fédération Internationale
Aéronautique (FAI). CIVL produce FS. It is the work of paid and volunteer
developers.

The scoring method is [well
documented](http://fs.fai.org/trac/wiki/ScoringFormulas). Principal documents
are;

* [Sporting Code Section 7A - Annex
  GAP](https://www.fai.org/sites/default/files/documents/sporting_code_s7a-xc-civl_gap_annex_1.pdf)
  - The definitive guide produced and maintained by CIVL.
* [GAP
  2002](http://fs.fai.org/trac/raw-attachment/wiki/ScoringFormulas/GAP02_en.pdf)
  - A much shorter and less formal guide that is a good first read to gain an
  understanding of why GAP is needed and how it works.

**Flare Timing** provides a reference implementation of GAP and shows the
working of how a pilot's score is calculated.

## Usage

See the separate discussion of [building](BUILDING.md) and
[testing](TESTING.md).

With an FS comp file and related track logs as inputs, **score** the comp;
1. Extract the inputs with
[`extract-input`](flare-timing/prod-apps/extract-input).  
2. Trace the shortest path to fly a task with
[`task-length`](flare-timing/prod-apps/task-length).  
3. Find pairs of fixes crossing over zones with
[`cross-zone`](flare-timing/prod-apps/cross-zone).  
4. Interpolate between crossing fixes for the time and place where a track tags
a zone with [`tag-zone`](flare-timing/prod-apps/tag-zone).  
5. Index fixes from the time of first crossing with
[`align-time`](flare-timing/prod-apps/align-time).  
6. Discard fixes that get further from goal and note leading area with
[`discard-further`](flare-timing/prod-apps/discard-further).  
7. Mask a task over its tracklogs with
[`mask-track`](flare-timing/prod-apps/mask-track).  
8. Group and count land outs with
[`land-out`](flare-timing/prod-apps/land-out).  
9. Score the competition with [`gap-point`](flare-timing/prod-apps/gap-point).  

Documentation is available online at
[flare-timing.readthedocs.io](http://flare-timing.readthedocs.io/) and there's
a [worked example](EXAMPLE.md) too.

## License

Copyright Phil de Joux 2017-2018

This software is subject to the terms of the Mozilla Public License, v2.0. If
a copy of the MPL was not distributed with this file, you can obtain one at
http://mozilla.org/MPL/2.0/.
