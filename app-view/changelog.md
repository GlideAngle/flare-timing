The [latest
version](https://github.com/BlockScope/flare-timing/blob/master/app-view/changelog.md)
of this changelog.

# IGC Parsing, Start Gates and Pilot Status
## app-view-0.

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
