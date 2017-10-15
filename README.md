# Flare Timing

**Flare Timing** will show a pilot's competition score in detail but is currently a work in progress.

The official scoring program for hang gliding and paragliding competitions is [FS](http://fs.fai.org/). Its principal output for a competition task is an html table of pilot scores. In addition to the day's scores, there are columns in this table for the time on course, the distance flown and for the breakdown of the scored points among distance points, lead points, time points and arrival points.

Looking for examples? There is a good archive of past Forbes Flatlands [competition results](http://www.forbesflatlands.com/results/past-results), for instance results for [task 1](https://www.forbesflatlands.com/results-show?id_results=7&db=results2013&class=results_open) of the 19th World Hang Gliding Champs.

Commission Internationale de Vol Libre (CIVL - Hang Gliding and Paragliding Commission) is an Air Sport Commission (ASC) of the Fédération Internationale Aéronautique (FAI). CIVL produce FS. It is the work of paid and volunteer developers.

The scoring method is [well documented](http://fs.fai.org/trac/wiki/ScoringFormulas). Principal documents are;

* [Sporting Code Section 7A - Annex GAP](http://www.fai.org/downloads/civl/SC7A_GAP) - The definitive guide produced and maintained by CIVL.
* [GAP 2002](http://fs.fai.org/trac/raw-attachment/wiki/ScoringFormulas/GAP02_en.pdf) - A much shorter and less formal guide that is a good first read to gain an understanding of why GAP is needed and how it works.

**Flare Timing** provides a reference implementation of GAP and shows the working of how a pilot's score is calculated.

## Usage

See the separate discussion of [building](BUILDING.md) and [testing](TESTING.md).

FSDB is the database of FS and is an XML format for inputs, working and outputs of scoring. To host a **Flare Timing** web site, start with an FSDB file and relative tracklog files on disk and run a pipeline of command line programs to produce the data to display then host the web services and web app.

1. Extract the inputs with [`extract-task`](flare-timing/prod-apps/extract-task).  
Produces a `*.comp-input.yaml` file.
2. Trace the shortest path to fly a task with [`task-length`](flare-timing/prod-apps/task-length).  
Produces a `*.task-length.yaml` file.
3. Find pairs of fixes crossing over zones with [`cross-zone`](flare-timing/prod-apps/cross-zone).  
Produces a `*.cross-zone.yaml` file.
4. Interpolate between crossing fixes for the time and place where a track tags a zone with [`tag-zone`](flare-timing/prod-apps/tag-zone).  
Produces a `*.tag-zone.yaml` file.
5. Find the nearest a track came to missing the next zone on course with [`near-miss`](flare-timing/prod-apps/near-miss).  
Produces a `*.near-miss.yaml` file.
6. Mask the competition task over the tracklogs with [`mask-track`](flare-timing/prod-apps/mask-track).  
Produces a `*.mask-track.yaml` file.
7. Time align the distance to goal for each fix with `leading-area`.  
Produces a `*.leading-area.yaml` file.
8. Score the competition with `gap-point`.  
Produces a `*.gap-point.yaml` file.
9. Start the [`server`](flare-timing/prod-apps/app-serve) hosting the web services.
10. Start the [`dev server`](flare-timing/view) or otherwise host the web app.
