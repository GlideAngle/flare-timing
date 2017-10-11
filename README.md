# Flare Timing

**Flare Timing** will show a pilot's competition score in detail but is currently a work in progress.

The official scoring program for hang gliding and paragliding competitions is [FS](http://fs.fai.org/). Its principal output for a competition task is an html table of pilot scores. There is a good archive of past Forbes Flatlands [competition results](http://www.forbesflatlands.com/results/past-results), for instance results for [task 1](https://www.forbesflatlands.com/results-show?id_results=7&db=results2013&class=results_open) of the 19th World Hang Gliding Champs.

As well as the day's score, the time on course and distance is shown as is the breakdown of the score among distance points, lead points, time points and arrival points.

Commission Internationale de Vol Libre (CIVL - Hang Gliding and Paragliding Commission) is an Air Sport Commission (ASC) of the Fédération Internationale Aéronautique (FAI). CIVL produce FS. It is the work of paid and volunteer developers.

The scoring method is [well documented](http://fs.fai.org/trac/wiki/ScoringFormulas). Principal documents are;

* [Sporting Code Section 7A - Annex GAP](http://www.fai.org/downloads/civl/SC7A_GAP) - The definitive guide produced and maintained by CIVL.
* [GAP 2002](http://fs.fai.org/trac/raw-attachment/wiki/ScoringFormulas/GAP02_en.pdf) - A much shorter and less formal guide that is a good first read to gain an understanding of why GAP is needed and how it works.

**Flare Timing** provides a reference implementation of GAP and shows the working of how a pilot's score is calculated.

## Usage

See the separate discussion of [building](BUILDING.md) and [testing](TESTING.md).

FSDB is the database of FS and is an XML format for inputs, working and outputs of scoring. To host a **Flare Timing** web site, start with an FSDB file and relative tracklog files on disk and run a pipeline of command line programs to produce the data to display.

1. Extract the inputs with [`extract-task`](flare-timing/prod-apps/extract-task) producing a `*.comp-input.yaml` file.
2. Workout the optimal distance to fly a task with [`task-length`](flare-timing/prod-apps/task-length) producing
   a `*.task-length.yaml` file.
3. Mask the competition task over the tracklogs with [`mask-track`](flare-timing/prod-apps/mask-track) producing
   a `*.mask-track.yaml` file.
4. Time align the distance to goal for each fix with `leading-area` producing
   a `*.leading-area.yaml` file.
5. Score the competition with `gap-point` producing a `*.gap-point.yaml` file.
6. Start the [`server`](flare-timing/prod-apps/app-serve) hosting the web services.
7. Start the [`dev server`](flare-timing/view) or otherwise host the web app.
