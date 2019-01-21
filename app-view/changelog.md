The [latest
version](https://github.com/BlockScope/flare-timing/blob/master/app-view/changelog.md)
of this changelog.

# Leading Points
## app-view-0.8

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
## app-view-0.7

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
## app-view-0.6

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
## app-view-0.5

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
## app-view-0.4

* Switch over to using megaparsec when parsing `*.igc` and `*.kml` files.
* Show pilots that did not fly (DNF) with absentees and scores.
* Fix an off-by-one error in equal placings.

```
-- 1,2=,2=,3
++ 1,2=,2=,4
```

# Pan to Zone
## app-view-0.3

Add a toggle button switching between zoom and pan. The map's button group now
has buttons for all zones, not just the speed section zones. The speed section
is indicated with color; green for start and red for stop.

# Zoom to Extent and Zoom to Zone
## app-view-0.2

Only one new feature in this release; a group of buttons above the map for
zooming in on each zone and for zooming to the extents of the task.

# Speed Section Velocity
## app-view-0.1

* Calculate the velocity over the speed section from the speed section distance
* Group headers using colour
* Show legs in the turnpoints table
* Show the optimal route on the map
* Added a layers control to the map
