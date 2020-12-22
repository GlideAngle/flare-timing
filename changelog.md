The [latest
version](https://github.com/BlockScope/flare-timing/blob/master/changelog.md)
of this changelog.

# Ess ≠ Goal
## v0.29

* Fixed a space leak and made some of the commands faster by making use of
  [parallel-io](https://hackage.haskell.org/package/parallel-io) and using
  pragmas; INLINE, INLINABLE and SPECIALIZE.
* Removed the `--dir` option from all commands. The current working directory
  is used instead.
* The `mask-track` command has been split up into smaller commands;
  `mask-arrival`, `mask-bonus`, `mask-effort`, `mask-lead` and `mask-reach`.
  Each deals with separate aspects of masking.
* Avoid calculating impossible means when the data set is empty. These were
  seen as huge negative numbers before the fix.
* Show the flying and scored fixes for pilots that have had their tracks
  downloaded for the map.
* Show the effective reset in the penal table when the pilot is too early.
* Move the penal tab to the top-level with its own children; Jump-the-Gun
  & Auto, Ess ≠ Goal and Manual.
* Under Basis | Pilots, put each kind of penalty in its own panel.
* Show toll, rebate and fee to explain how the points after applying the
  jump-the-gun penalty are not less than minimum distance.
* Score the penalty for making Ess but not making goal.
* Show tasks stopped and cancelled in the task list.
* Only show the extra reach via glide column for stopped tasks.
* Don't dislay a chunk distance in the effort table for pilots that don't land
  out.
* Parse `FsCompetition/@utc_offset` whether or not it has a leading sign for
  both integers and floats.
* Fixed a bug where looking up a chunk for effort failed because of rounding
  then the misplaced pilot got zero points for effort.
* Only the *.fsdb and folders for track logs are left in the root folder since
  the input, output and working files of flare-timing moved to the dot folder
  `.flare-timing`. Likewise files for comparison have moved from the root to
  `.flight-system` and `.air-score`. Add a `Compare` top-level tab for the
  comparisons between scores.

# Plot Selected Pilots
## v0.28

* Zero seconds per point is used by FS as a sentinel value to turn off the jump
  the gun penalty. Be aware of this and calculate the penalty only if the
  seconds per point are positive.
* Select the last crossing of the last start gate for the start crossing.
* Exclude start zone crossings that come after tagging subsequent zones.
* Selecting table rows will highlight points for those pilots in the scatter plots.
* Restrict function-plot to v1.19.1 as later versions introduce breaking changes.
* Use square of leading area when calculating leading points except for:
  * GAP2000
  * GAP2002
  * OzGAP2005
  * GAP2007
  * GAP2008
  * GAP2009
  * GAP2011
  * GAP2012
  * GAP2013
  * GAP2014
  * GAP2015
* Use positive numbers for demerits and negative numbers for penalties.
* Build the comp server with cabal by relaxing constraints on HUnit.
* Commands dealing with FSDB extraction already had `fs-` prefixes. Do the same
  for flare-timing, using `ft-` prefixes for these commands.
    ```
      > cabal install all:exes --overwrite-policy=always --installdir=$HOME/.cabal/bin
      ...
      Symlinking 'fs-arrival'
      Symlinking 'fs-effort'
      Symlinking 'ft-extract-input'
      Symlinking 'ft-land-out'
      Symlinking 'fs-filter'
      Symlinking 'ft-discard-further'
      Symlinking 'ft-cross-zone'
      Symlinking 'ft-tag-zone'
      Symlinking 'fs-score'
      Symlinking 'ft-mask-track'
      Symlinking 'ft-gap-point'
      Symlinking 'ft-task-length'
      Symlinking 'ft-align-time'
      Symlinking 'ft-peg-frame'
      Symlinking 'ft-area-step'
      Symlinking 'fs-route'
      Symlinking 'ft-unpack-track'
      Symlinking 'ft-build
      Symlinking 'ft-comp-serve
    ```
* Show penalties with a leading `+` or `-` prefix for additive, multiplicative
  and reset penalties to indicate whether this it's a bonus or penalty.

# Weighted Leading Area
## v0.27

* Show milliseconds if a pilot jumped the gun by less than a second.
* Display ###-pilot or ####-pilot dependening on the pilot id width.
* Add a marker to the nearest vertex on the nearest track line when the map is
  clicked. Use the default marker for these.
* Show all crossings in a layer on the map, the nominees. Use different marker
  icons for different items. Use unfilled circles for crossings and filled
  circles for taggings.
* Pick the first crossing of the start gate taken.
* Better detect whether a start is an entry or an exit.
* Associate crossings with start gates and pick the last crossing of the last
  start as the start.
* Show start nominee crossings for each start gate and the start crossing as
  separate layers on the map.

# Leading Area Plot
## v0.26

* Show a plot of the leading area, selecting up to five pilots at once. Include
  the extra area added after landing and another potential area hinted at in FS
  code comments, the area added before the start. Show the parts of the leading
  area in the legend of the plot.
* Show the leading area with thousands separator and units of km^2 s.
* Show the leading area difference as actual value over expected value minus one.
* Use distance or distance squared for the leading area.
* Show a table of tasks instead of a list of tasks.

# Open API Specification
## v0.25

* Added an OpenAPI specification ([OAS](https://swagger.io/)) for the server.

# Arrival Time Points and Start Too Early
## v0.24

* Score hang gliding comps using arrival time points.
* Document for stopped tasks at least that unpack-track needs to be run before
  peg-frame.
* Color code closeness to scores of FS in the task list using green for less
  than 2 points, yellow for less than 8 points and red for greater than that in
  the mean of the differences in the total points.
* Do not default to award arrival position points when no kind of arrival
  points are allowed.
* Award pilots without a tracklog effort for minimum distance.
* Add a table showing jump the gun penalties and how fractional, point and
  reset penalties are applied and the final points are rounded.
* Consistently strike out reach when the pilot was awarded minimum distance,
  whether that pilot had a track or not.

# Jump the Gun
## v0.23

* Show the task number before the task name in the breadcrumb.
* Allow early tagging if within the jump-the-gun window.
* Don't show all the turnpoint names in the task detail if they will overflow
  the header.
* Add a ruler to the map.
* Use zones enlarged by give, the fractional and absolute tolerance added to
  a zone's radius to give some relief to pilots with vario inaccuracy.
* Show the mean and standard deviation in the difference of task points
  compared to the expected points from FS.
* FS renamed `FsTaskShortestRoute/FsTurnpoint` to
  `FsTaskShortestPath/FsPathVertex`. Updated the `*.fsdb` parsing accordingly.

# Earth, Through and Through
## v0.22

* Use the Earth model and Earth math throughout the scoring.
* Avoid skipping some B records (position fixes) when parsing `*.igc` files.
* Show the effective inside and outside border of turnpoints with tolerance
  applied.
* Allow a crossing if one fix passes the time check but the other does not.

# Validity Compared
## v0.21

* Compare task validities side-by-side with those of FS.
* Show the optimal task route from FS on the map.
* Compare optimal task route from FS with those of FS in the Geo table.
* Show the extra distance from glide above goal in the score distance table.
* Compare reach and effort with linear and difficulty from FS in the score
  distance table, in the tables beside the reach and effort plots.
```
.
└── points
    ├── distance_points
    │   ├── linear_distance_points
    │   └── difficulty_distance_points
    ├── time_points
    ├── arrival_points
    ├── leading_points
    ├── departure_points
    └── penalty_points
```
* Handle glitches in the ordering `*.igc` file B record times. Time is meant to
  only increment during the day as fixes are logged. Discard times going
  backward less than an hour and roll over the day when the time goes backwards
  more than that. There's no rule in the GAP document about this but that is
  what FS does.
* Exclude zone crossings that fall outside opening times of the zones.
* Workout task difficulty using relative difficulty from every chunk of the
  task, not just those where a pilot has landed.
* Add score tables for effort, speed, time and arrival. In these, sort the rows
  by descreasing order of the relevant points.
* Include the pilot race number with the pilot in each ###-Pilot column of
  tables.
* Enlarge the split plot.
* Squish and stretch the leading plot to make room for the width of the
  associated table.
* Switch to using the B612 font, designed for use in aircraft cockpit screens.
* Add fs-effort, a command line tool for extracting the landings, chunking and
  relative difficulties from a competition `*.fsdb`.
* Show two more decimal places when point difference is +0.0 or -0.0.
* Clean and trim elements and attributes of the `*.fsdb` before use with
  `fs-filter`, a new command line app that writes `*.clean-fsdb.xml` and
  `*.trim-fsdb.xml`. The cleaned file has sensitive information removed that
  would allow its publication. The trimmed file is the minimal set required by
  flare-timing, taking some data as inputs for scoring and other data for
  comparisons of the scores.

# Stopped Validity and Bonus Reach
## v0.20

* Only consider crossings that are exiting exit zones and entering entry zones.
* Compare arrival fractions between flare-timing and FS.
* Show the working for weight calculations next to the split plot.
* For paragliding, when the goal ratio is zero use a tenth of the distance
  ratio for the leading weight.
* Show the stopped task validity.
* Add plots for launch, distance and time validity.
* Add a series of three plots for stop validity relative to mean reach, the
  spread of reach and the number of pilots landed before the task is stopped.
* Peg the data frame for each pilot's track to the scored time window for
  stopped tasks.
* When the task is stopped, add a bonus distance to reach. Glide from the
  altitude above goal to the altitude of goal for each fix of the track,
  picking the shifted fix that is now closest to goal as the point reached.

# Making a Line Goal
## v0.19

* Sample line goals as semicircles when figuring out the optimal path.
* Sort and rename the base layers of the map to:

        Task
        Path (spherical)
        Path (ellipsoid)
        Path (planar)
        Race (spherical subset of path)
        Race (ellipsoid subset of path)
        Race (spherical subset of waypoints)
        Race (ellipsoid subset of waypoints)
* Detect crossings of line goals.
* Show the list of tasks even when task lengths have not been computed.
* Show the task name in the list of tasks.

# Interpolate Zone Tagging
## v0.18

* Find that GAP and FS use different equations for working out the speed
  fraction and leading fraction. Show the curve of the equation in the GAP
  rules but use the same equation as FS.
* Add a column for leading area in the table alongside the leading point
  distribution graph and compare area, coefficient and fraction with norms.
* When the HH:MM:SS field of IGC B records decrease bump the YYYY-MM-DD.
* Don't apply give to exit cylinders as pilots often fly back into them to take
  a start. They're flown as both entry and exit cylinders.
* When comparing data to norms, if the numbers aren't exactly the same but the
  rounded display of numbers are then display "=" instead.
* Interpolate zone tagging and show pairs of fixes that straddle a zone and the
  tag point as separate markers on the map.
* Draw line goals on the map as semicircles.

# Side-by-Side Score Check
## v0.17

* Parse the `*.fsdb` for scores.
* Add an `Overview | Points | Speed | Distance` set of child tabs beneath the
  `Score` tab. On the **Overview** table display the ranking and total points
  from FS side-by-side with the scores from flare-timing. On the **Points**
  table show side-by-side breakdown of points between FS and flare-timing. On
  the **Speed** table do the same for the components of velocity and on the
  **Distance** table do the same for the components of distance points.

# Effort Plot
## v0.16

* Add a plot for effort.

# Fewer Tabs
## v0.15

* Reduce the number of top level tabs to `Task | Map | Score | Plot | Basis`.
* Select the pilot tracklog for display on the map when fetched.

# Singular Plots
## v0.14

* Default the x-domain when there is one data point to plot.
* Pad the x-domain of the plots relative to the range of the data.
* Add a reach plot.

# Fractional Point Plots
## v0.13

* Add plots for time, arrival and leading point fractions.
* Show the working for task validity.
* Fix errors transcribing the time fraction and leading fraction formulae to code.
* Use color swatches with plot tabs.

# Duplicate Zones and Angle Formats
## v0.12

* Fix a bug showing turnpoints when the first zone starts the speed section.
* When time rolls over in a sequence of IGC B records, bump the date.
* Allow for negative altitudes in IGC B records.
* Allow duplicate zones when working out task distance.
* Detect when pilots jump the gun. The penalty is not yet applied.
* Show the arrival ratio in the legend of the split points plot.
* Show the tiles by default on the map.
* Switch from opentopomap to openstreetmap for the tiles on the map.
* Parse the lat and lng of turnpoints in `*.fsdb` files as ddd or dmm or dms.

# Split of Available Points
## v0.11

* Add a plot of curves showing how the weighting for distance points, for time
  points, for arrival points and for leading points changes in relation to the
  goal ratio. On this show a vertical line showing the weights of the task at
  hand.

# Give a Little
## v0.10

* Score with the give in turnpoint radius.
* Use the last pilot landing time in leading points calculations and allow for
  leading points when no pilots complete the speed section.
* Pilots with no tracks can be assigned speed section times by the scorer and
  thereby get time points and arrival points. Show these assignments.
* Apply percentage and absolute point penalties to pilot scores and show the
  penalty reason.
* Pilots can share arrival position.

# Associate Fixes with Legs
## v0.9

* Better identify which leg each fix belongs to.
* Show the distances to goal in the turnpoints table.
* Show the penalties associated with each pilot.
* Parse when leading and arrival points are off for a task.
* Show the pilots altitude at the score back time for a stopped task.
* Remove duplicate fixes in the tracklog due to ignored sub-second logging.

# Leading Points
## v0.8

* Pilots landing out get their leading points scaled in the same way as pilots
  making goal.
* Fix a bug counting some pilots twice resulting in too large a denominator in
  the goal ratio fraction.
* Parse `FsScoreFormula/@double_leading_weight` as a scaling for leading
  weight, ignoring it if it matches the default for the discipline.
* Score a stopped task.
* Fix a bug adding pilot tracks to the layers control more than once each.
* Show the unscored part of a pilot's track on the map for a stopped task.
* Show when a task was stopped and when it will be scored back to.

# Trackless Pilots
## v0.7

* Parsing `*.fsdb` files for pilots that do not have an associated tracklog
  file that did fly. These pilots will be awarded either the competition
  minimum distance or a another distance decided by the scorer.
* Show when minimum distance was awarded over a lesser flown distance.
* Show which pilots were scored without tracklogs.
* Show that arrival points and difficulty points are not awarded in
  paragliding.
* Pick the most adjacent start gate that opened before the pilot started.
* Show when there are no start gates.
* Show as much as can be shown about tasks that not been flown or scored.
* Show various speed section routes on the map, each calculated differently.
* Add a **Geo** tab showing task distances calculated by various Earth models
  and algorithms.

# Pilot Tracks and Zone Give
## v0.6

* Download and show pilot tracks on the map.
* Show the give or tolerance around turnpoints both in the table an on the map.
* Read and display zone altitude when available.
* Show the shape of the end of the speed section and the shape of goal in the
  turnpoint table.
* Add a settings tab for the competition showing configurations such as the
  Earth model and distance calculation method. Some of these settings are not
  available for extraction from the `*.fsdb` and instead are provided as
  command line options when doing the `extract-input` scoring step.
* Fix a bug in the sampling of zone shapes when setting up the path graph for
  working out the optimal route, #111.
* Fix a bug in showing the legs of the task, #145.

# IGC Parsing, Start Gates and Pilot Status
## v0.5

* Parse the newer `*.igc` **HFDTEDATE:** date header.
* Show pilots that are not yet processed (NYP).
* Show task start gates.
* Show turnpoint open and close times.
* Show competition minimum distance and use this in scoring.
* Show competition score back time.
* Show alternative speed section routes on the map.
* For the point-to-point course line show the turnpoint name but for
  alternative routes show the lat/lng of each waypoint in the marker popup.
* When scoring is incomplete, show the tasks anyway.

# Equal Placings and DNF
## v0.4

* Switch over to using megaparsec when parsing `*.igc` and `*.kml` files.
* Show pilots that did not fly (DNF) with absentees and scores.
* Fix an off-by-one error in equal placings.

```
-- 1,2=,2=,3
++ 1,2=,2=,4
```

# Pan to Zone
## v0.3

Add a toggle button switching between zoom and pan. The map's button group now
has buttons for all zones, not just the speed section zones. The speed section
is indicated with color; green for start and red for stop.

# Zoom to Extent and Zoom to Zone
## v0.2

Only one new feature in this release; a group of buttons above the map for
zooming in on each zone and for zooming to the extents of the task.

# Speed Section Velocity
## v0.1

* Calculate the velocity over the speed section from the speed section distance
* Group headers using colour
* Show legs in the turnpoints table
* Show the optimal route on the map
* Added a layers control to the map
