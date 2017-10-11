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
3. Mask the competition task over the tracklogs with `mask-track` producing
   a `*.mask-track.yaml` file.
4. Time align the distance to goal for each fix with `leading-area` producing
   a `*.leading-area.yaml` file.
5. Score the competition with `gap-point` producing a `*.gap-point.yaml` file.

Once the data is prepared the server web service and single page client web app, the comp server and comp client, can be started.
      
### Masking Tracks

    $ __shake-build/mask-track --help
    Given a competition input YAML file, *.comp-input.yaml, and relative track log
    KML files, by masking the track logs with the zones, work out;
    * if the pilot launched
    * if they made goal then
        * how long the pilot took to reach goal
    * if they landed out then
        * how far they got along the course
        * how far yet to reach goal

    mask-track [OPTIONS]

    Source:
      -d --dir=ITEM            Over all the competition *.comp.yaml files in this
                               directory
      -f --file=ITEM           With this one competition *.comp.yaml file
    Filter:
      -t --task[=TASK NUMBER]  Which tasks?
      -p --pilot[=PILOT NAME]  Which pilots?
      -r --reckon=RECKON NAME  Work out one of these things,
                               launch|goal|zones|goaldistance|flowndistance|time|lead
      -m --measure=METHOD      Which way to measure task distances,
                               taskdistancebyallmethods|taskdistancebypoints|taskdistancebyedges

### Leading Area

TODO

### GAP Points

TODO

## Web Apps

### Comp Server

    $ __shake-build/comp-serve --help
    Serve nominals, tasks and pilots from a competition YAML file.

    comp-serve [OPTIONS]
      With one competition *.comp.yaml file supplied

    Common flags:
      -f --file=ITEM
      
Run the server;

    $ __shake-build/comp-serve --file=Forbes2012.comp.yaml
    Drive {file = "Forbes2012.comp.yaml"}
    ServeOptions {file = "Forbes2012.comp.yaml"}
    listening on port 3000

The following are the web service endpoints;

    http://localhost:3000/nominals
    http://localhost:3000/tasks
    http://localhost:3000/pilots

### Comp Client

Start the webpack devserver and navigate to `http://localhost:9000/app.html` for the client;

    ./build.sh view-start
    
    $ webpack-dev-server
    Project is running at http://localhost:9000/
