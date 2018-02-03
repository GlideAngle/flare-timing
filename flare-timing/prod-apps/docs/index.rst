.. Flare Timing documentation master file, created by
   sphinx-quickstart on Sat Feb  3 10:15:05 2018.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Welcome to Flare Timing's documentation!
========================================

The `flare-timing <https://github.com/BlockScope/flare-timing>`_
implementation of GAP is a set of command line apps to be run in
sequence. The workings of each step and the final scores are written to
plain-text files. In these there’s enough detail and supporting evidence
for everything to be checked by hand.

Let’s now walk through the process of scoring with ``flare-timing``, see
Fig \ `[fig:flare-timing] <#fig:flare-timing>`__, and look at snippets
of the files it writes.

.. toctree::
   :maxdepth: 2
   :caption: Contents:

Indices and tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`

Let’s now walk through the process of scoring with ``flare-timing``, see
Fig \ `[fig:flare-timing] <#fig:flare-timing>`__, and look at snippets
of the files it writes.

.. raw:: latex

   \centering

Starting with an ``*.fsdb`` comp and related ``*.igc`` or ``*.kml``
track logs, scoring proceeds in steps  [1]_;

#. Extract the inputs with
   `extract-input <https://github.com/BlockScope/flare-timing/tree/master/flare-timing/prod-apps/extract-input>`__.

#. Trace the route of the shortest path to fly a task with
   `task-length <https://github.com/BlockScope/flare-timing/tree/master/flare-timing/prod-apps/task-length>`__.

#. Find pairs of fixes crossing over zones with
   `cross-zone <https://github.com/BlockScope/flare-timing/tree/master/flare-timing/prod-apps/cross-zone>`__.

#. Interpolate between crossing fixes for the time and place where a
   track tags a zone with
   `tag-zone <https://github.com/BlockScope/flare-timing/tree/master/flare-timing/prod-apps/tag-zone>`__.

#. Index fixes from the time of first crossing with
   `align-time <https://github.com/BlockScope/flare-timing/tree/master/flare-timing/prod-apps/align-time>`__.

#. Discard fixes that get further from goal and note leading area with
   `discard-further <https://github.com/BlockScope/flare-timing/tree/master/flare-timing/prod-apps/discard-further>`__.

#. Mask a task over its tracklogs with
   `mask-track <https://github.com/BlockScope/flare-timing/tree/master/flare-timing/prod-apps/mask-track>`__.

#. Group and count land outs with
   `land-out <https://github.com/BlockScope/flare-timing/tree/master/flare-timing/prod-apps/land-out>`__.

#. Score the competition with
   `gap-point <https://github.com/BlockScope/flare-timing/tree/master/flare-timing/prod-apps/gap-point>`__.

.. raw:: latex

   \newpage

Extracting Inputs
-----------------

In the ``*.fsdb`` FS keeps both inputs and outputs. We’re only
interested in a subset of the input data, just enough to do the
scoring [2]_;

Competition
    id, name, location, dates and UTC offset.

Nominal
    launch, goal, time, distance and minimal distance.

Task
    name and type of task, zones, speed section, start gates and pilots.

Zone
    name, latitude, longitude, altitude and radius.

Pilot
    name and either absentee status or track log file name.

Something to be aware of when parsing ``XML`` of ``*.fsdb`` is that
attributes may be missing and in that case we’ll have to infer the
defaults used by FS. This is done by looking at the source code of FS as
there is no schema for the ``XML`` that could also be used to set
default values.

.. code:: xml

    <Fs>
      <FsCompetition id="7592" name="2012 Hang Gliding Pre-World Forbes" location="Forbes, Australia"
          from="2012-01-05" to="2012-01-14" utc_offset="11">
        <!-- Nominals are set once for a competition but beware, they are repeated per task. -->
        <FsScoreFormula min_dist="5" nom_dist="80" nom_time="2" nom_goal="0.2" />
        <FsParticipants>
          <FsParticipant id="23" name="Gerolf Heinrichs" />
          <FsParticipant id="106" name="Adam Parer" />
        </FsParticipants>
          <!-- Flags on how to score are also set for the competition but pick them up from the task. -->
          <FsTask name="Day 8" tracklog_folder="Tracklogs\day 8">
            <FsScoreFormula use_distance_points="1" use_time_points="1" use_departure_points="0" use_leading_points="1" use_arrival_position_points="1" use_arrival_time_points="0" />
            <FsTaskDefinition ss="2" es="5" goal="LINE" groundstart="0">
              <!-- Not shown here but each FsTurnpoint has open and close attributes. -->
              <FsTurnpoint id="FORBES" lat="-33.36137" lon="147.93207" radius="100" />
              <FsTurnpoint id="FORBES" lat="-33.36137" lon="147.93207" radius="10000" />
              <FsTurnpoint id="MARSDE" lat="-33.75343" lon="147.52865" radius="5000" />
              <FsTurnpoint id="YARRAB" lat="-33.12908" lon="147.57323" radius="400" />
              <FsTurnpoint id="DAY8GO" lat="-33.361" lon="147.9315" radius="400" />
              <!-- This was an elapsed time task so no start gates. -->
            </FsTaskDefinition>
            <FsTaskState stop_time="2012-01-14T17:22:00+11:00" />
            <FsParticipants>
              <!-- An empty element is an absent pilot who did not fly (DNF) the task. -->
              <FsParticipant id="106" />
              <FsParticipant id="23">
                <FsFlightData tracklog_filename="Gerolf_Heinrichs.20120114-100859.6405.23.kml" />
              </FsParticipant>
            </FsParticipants>
          </FsTask>
        </FsTasks>
      </FsCompetition>
    </Fs>

.. raw:: latex

   \newpage

Tracing an Optimal Route
------------------------

To find the best route ``flare-timing`` constructs a graph and finds the
shortest path connecting the nodes. It puts nodes on turnpoint cylinder
arc boundaries and uses the haversine distance as the cost of connecting
nodes in the network. It would be expensive to construct and evaluate a
large network with the accuracy required so in an iterative process, as
the arc of the circle is shortened, getting closer to the optimal
crossing point, the density of nodes is increased. All happening on the
FAI sphere, this is the edge to edge optimal route. Routes are shown
with waypoints, segment distances  [3]_ and total distance.

::

    taskRoutes:
      edgeToEdge:
        distance: 159.373683
        legs:
        - 10.078208
        - 42.525217
        - 0
        - 64.949832
        - 41.820427
        legsSum:
        - 10.078208
        - 52.603424
        - 52.603424
        - 117.553256
        - 159.373683
        waypoints:
        - lat: -33.36047067
          lng: 147.93206999
        - lat: -33.43411056
          lng: 147.86878018
        - lat: -33.7159199
          lng: 147.55846831
        - lat: -33.7159199
          lng: 147.55846831
        - lat: -33.13199024
          lng: 147.57575486
        - lat: -33.35857718
          lng: 147.93468357

The naive way to measure task length would be to just connect the
centers of each control zone. This is the point to point distance.

::

    taskRoutes:
      pointToPoint:
        distance: 169.10714
        legs:
        - 57.427511
        - 69.547668
        - 42.131961
        legsSum:
        - 57.427511
        - 126.975179
        - 169.10714
        waypoints:
        - lat: -33.36137
          lng: 147.93207
        - lat: -33.75343
          lng: 147.52864998
        - lat: -33.12908
          lng: 147.57322998
        - lat: -33.36099999
          lng: 147.93149998

.. raw:: latex

   \newpage

Knowing that FS uses a plane to work out the shortest route in two
dimensions, on the the Universal Transverse Mercator projection, we can
also do that with our graph algorithm. We end up with waypoints, optimal
on the plane but possibly sub-optimal on the sphere.

::

    taskRoutes:
      projection:
        spherical:
          distance: 159.373683
          legs:
          - 10.078208
          - 42.525217
          - 0
          - 64.949832
          - 41.820427
          legsSum:
          - 10.078208
          - 52.603424
          - 52.603424
          - 117.553256
          - 159.373683
          waypoints:
          - lat: -33.36047067
            lng: 147.93206999
          - lat: -33.43411056
            lng: 147.86878018
          - lat: -33.7159199
            lng: 147.55846831
          - lat: -33.7159199
            lng: 147.55846831
          - lat: -33.13199024
            lng: 147.57575486
          - lat: -33.35857718
            lng: 147.93468357

::

    taskRoutes:
      projection:
        planar:
          distance: 159.144781
          legs:
          - 10.065441
          - 42.4942
          - 0
          - 64.761082
          - 41.820427
          legsSum:
          - 10.065441
          - 52.559642
          - 52.559642
          - 117.320723
          - 159.14115
          mappedPoints:
          - easting: 586715.834
            northing: 6308362.198
          - easting: 580759.282
            northing: 6300248.47
          - easting: 551744.701
            northing: 6269201.551
          - easting: 551744.701
            northing: 6269201.551
          - easting: 553704.761
            northing: 6333932.964
          - easting: 586960.882
            northing: 6308569.955
          mappedZones:
          - latZone: H
            lngZone: 55

.. raw:: latex

   \newpage

Finding Zone Crossings
----------------------

Before we can determine if any zones have been crossed we’ll have to
decide how to tell which parts of a track log are flown and which are
walked or driven in the retrieve car, possibly even back to goal. [4]_

To work out when a pilot is flying, select the longest run of fixes that
are not the same allowing for some stickiness when the GPS loses signal.
For example we might consider within ± 1m altitude or within ±
1/10,000th of a degree of latitude or longitude to be in the same
location and not likely recorded during flight.

::

    flying:
      - - Gerolf Heinrichs
        - loggedFixes: 4786
          flyingFixes:
          - 293
          - 4775
          loggedSeconds: 19140
          flyingSeconds:
          - 1172
          - 19100
          loggedTimes:
          - 2012-01-14T02:00:05Z
          - 2012-01-14T07:19:05Z
          flyingTimes:
          - 2012-01-14T02:19:37Z
          - 2012-01-14T07:18:25Z

Next we need to find and nominate every crossing of each control zone
and from among those work out which pair to select as the zone crossing.
The same control zone may be crossed multiple times and we need a
sequence of crossings ordered in time that fits the task [5]_.

::

    crossing:
      - - Gerolf Heinrichs
        - zonesCrossSelected:
          - crossingPair: ...
          - crossingPair: ...
          - crossingPair: ...
          - crossingPair: ...
          - crossingPair: ...
            - fix: 4711
              time: 2012-01-14T07:14:09Z
              lat: -33.35869799
              lng: 147.927904
            - fix: 4712
              time: 2012-01-14T07:14:13Z
              lat: -33.35934199
              lng: 147.928741
            inZone:
            - false
            - true
        - zonesCrossNominated: ...

.. raw:: latex

   \newpage

Interpolating Zone Taggings
---------------------------

Between the pair of fixes straddling a control zone, we need to
interpolate the point at which the pilot is most likely to have crossed
and the time of this tagging of the turnpoint [6]_.

::

    tagging:
      - - Gerolf Heinrichs
        - zonesTag:
          - fix: 294
            time: 2012-01-14T02:19:41Z
            lat: -33.36058598
            lng: 147.93161599
          - fix: 1367
            time: 2012-01-14T03:31:13Z
            lat: -33.41472398
            lng: 147.846
          - fix: 2208
            time: 2012-01-14T04:27:17Z
            lat: -33.708522
            lng: 147.530401
          - fix: 3915
            time: 2012-01-14T06:21:05Z
            lat: -33.13216898
            lng: 147.57301598
          - fix: 4712
            time: 2012-01-14T07:14:13Z
            lat: -33.35934198
            lng: 147.928741

Sorting the list of tagging times, we can show the first and last times,
the count of taggings and the pilots.

::

    timing:
    - zonesSum:
      - 76
      - 83
      - 75
      - 56
      - 29
      zonesFirst:
      - 2012-01-14T02:00:37Z
      - 2012-01-14T02:43:04Z
      - 2012-01-14T04:26:06Z
      - 2012-01-14T06:21:05Z
      - 2012-01-14T07:14:13Z
      zonesLast:
      - 2012-01-14T03:10:03Z
      - 2012-01-14T04:19:08Z
      - 2012-01-14T07:16:55Z
      - 2012-01-14T08:06:43Z
      - 2012-01-14T08:12:35Z
      zonesRankTime:
      - - ...
      - - 2012-01-14T07:14:13Z
        - 2012-01-14T07:30:57Z
        - 2012-01-14T07:35:05Z
        - ...
      zonesRankPilot:
      - - ...
      - - Gerolf Heinrichs
        - Attila Bertok
        - Jonas Lobitz
        - ...

.. raw:: latex

   \newpage

Aligning Tracks by Elapsed Time
-------------------------------

Next we align the tracks in time elapsed from the first start and work
out the distance flown for each fix.

::

    leg,time,lat,lng,tickLead,tickRace,distance
    0,2012-01-14T02:19:37Z,-33.36082199,147.93187399,-1407.0,-1407.0,159.275
    1,2012-01-14T02:19:41Z,-33.36058599,147.93161599,-1403.0,-1403.0,159.279
    ...
    1,2012-01-14T03:31:09Z,-33.41410199,147.84640799,2885.0,2885.0,149.643
    2,2012-01-14T03:31:13Z,-33.41472399,147.84600000,2889.0,2889.0,149.565
    ...
    2,2012-01-14T04:27:13Z,-33.70794300,147.53052900,6249.0,6249.0,106.253
    3,2012-01-14T04:27:17Z,-33.70852200,147.53040100,6253.0,6253.0,106.066
    ...
    3,2012-01-14T06:21:01Z,-33.13285599,147.57273699,13077.0,13077.0,42.015
    4,2012-01-14T06:21:05Z,-33.13216899,147.57301599,13081.0,13081.0,41.938
    ...
    4,2012-01-14T07:14:09Z,-33.35869799,147.92790400,16265.0,16265.0,0.421
    5,2012-01-14T07:14:13Z,-33.35934198,147.92874100,16269.0,16269.0,0.0

Discarding Fixes further from Goal
----------------------------------

Then we discard any fixes that get further from goal and work out the
leading area for each increment of distance.

::

    leg,tickLead,tickRace,distance,area
    0,-1407.0,-1407.0,159.275,0.0
    1,-1367.0,-1367.0,159.264,0.0
    ...
    1,2885.0,2885.0,149.643,0.0
    2,2889.0,2889.0,149.565,1.4747201784719584e-9
    ...
    2,6249.0,6249.0,106.253,1.2492010001497237e-9
    3,6253.0,6253.0,106.066,5.430166826250327e-9
    ...
    3,13077.0,13077.0,42.015,1.6116016630161737e-9
    4,13081.0,13081.0,41.938,1.8495333543888993e-9
    ...
    4,16265.0,16265.0,0.421,3.46272001957514e-11
    5,16269.0,16269.0,0.0,6.306949125520327e-11

.. raw:: latex

   \newpage

Masking Task over Track
-----------------------

Taking what we now know about the tracks and the task, we have times,
distances and fractions we’ll need later for the points. So far we have
leading, arrival and speed fractions but we still need to look at where
pilots landed to have the distance fractions.

::

    pilotsAtEss:
    - 29
    raceTime:
    - openTask: 2012-01-14T01:00:00Z
      closeTask: 2012-01-14T09:00:00Z
      firstStart: 2012-01-14T02:43:04Z
      firstLead: 2012-01-14T02:43:04Z
      lastArrival: 2012-01-14T08:12:35Z
      leadArrival: 19771
      leadClose: 22616
      openClose: 28800
      tickClose: 22616
    bestTime:
    - 3.716666 h
    taskDistance:
    - 159.374
    bestDistance:
    - 159.374
    sumDistance:
    - 9427.028999999999
    minLead:
    - 3.62e-6
    lead:
    - - - Gerolf Heinrichs
        - coef: 3.62e-6
          frac: 1
      - - Jonas Lobitz
        - coef: 3.67e-6
          frac: 0.99915269
      - - Attila Bertok
        - coef: 3.76e-6
          frac: 0.99828003
      ...
    arrival:
    - - - Gerolf Heinrichs
        - rank: 1
          frac: 1
      - - Attila Bertok
        - rank: 2
          frac: 0.92666251
      - - Jonas Lobitz
        - rank: 3
          frac: 0.8579945
      ...
    speed:
    - - - Gerolf Heinrichs
        - time: 3.716666 h
          frac: 1
      - - Curt Warren
        - time: 3.865 h
          frac: 0.81909886
      - - Peter Dall
        - time: 3.921388 h
          frac: 0.77575361
      ...

.. raw:: latex

   \newpage

For those landing out, how close or nigh were they to goal and where did
they land?

::

    nigh:
      - - Phil de Joux
        - togo:
            distance: 113.088594
            legs:
            - 7.018315
            - 64.249853
            - 41.820427
            legsSum:
            - 7.018315
            - 71.268168
            - 113.088594
            waypoints:
            - lat: -33.65111698
              lng: 147.560333
            - lat: -33.70846391
              lng: 147.52864998
            - lat: -33.13199024
              lng: 147.57575486
            - lat: -33.35857718
              lng: 147.93468357
          made: 46.285
      - - Hadewych van Kempen
        - togo:
            distance: 123.733462
            legs:
            - 17.663183
            - 64.249853
            - 41.820427
            legsSum:
            - 17.663183
            - 81.913035
            - 123.733462
            waypoints:
            - lat: -33.577367
              lng: 147.6364
            - lat: -33.70846391
              lng: 147.52864998
            - lat: -33.13199024
              lng: 147.57575486
            - lat: -33.35857718
              lng: 147.93468357
          made: 35.641
    land:
      - - Phil de Joux
        - togo: 113.129
          made: 46.245
      - - Hadewych van Kempen
        - togo: 124.013
          made: 35.361

.. raw:: latex

   \newpage

Assessing Difficulty
--------------------

We consider only pilots landing out in 100m chunks of the task. How many
came down in a chunk and how many are downward bound is shown. For each
chunk with a landing, we have its relative difficulty. Taking the sum of
these under a moving window we have difficulty fractions to award to
pilots.

::

    minDistance: 5.0 km
    bestDistance:
    - 159.3 km
    landout:
    - 55
    lookahead:
    - 87
    sumOfDifficulty:
    - 268
    - - chunk: 20
        startChunk: 6.9 km
        endChunk: 7.0 km
        endAhead: 15.7 km
        down: 1
        downward: 2
        rel: 3.73134e-3
        frac: 3.73134e-3
      - chunk: 85
        startChunk: 13.4 km
        endChunk: 13.5 km
        endAhead: 22.1 km
        down: 1
        downward: 3
        rel: 5.59701e-3
        frac: 9.32835e-3
      - chunk: 121
        startChunk: 17.0 km
        endChunk: 17.1 km
        endAhead: 25.8 km
        down: 1
        downward: 5
        rel: 9.32835e-3
        frac: 1.865671e-2
      ...
      - chunk: 1483
        startChunk: 153.2 km
        endChunk: 153.3 km
        endAhead: 162.0 km
        down: 1
        downward: 3
        rel: 5.59701e-3
        frac: 0.49440298
      - chunk: 1486
        startChunk: 153.5 km
        endChunk: 153.5 km
        endAhead: 162.3 km
        down: 1
        downward: 2
        rel: 3.73134e-3
        frac: 0.49813432
      - chunk: 1499
        startChunk: 154.8 km
        endChunk: 154.9 km
        endAhead: 163.6 km
        down: 1
        downward: 1
        rel: 1.86567e-3
        frac: 0.5

.. raw:: latex

   \newpage

Collating Scores
----------------

To calculate validites, we need nominal values, the fraction of pilots
flying, the best distance and the best time and the sum of the distance
flown. From the ratio of pilots making goal we can work out the weights
and then with the validities we can work out the available points.

::

    validityWorking:
    - time:
        bestDistance: 159.3 km
        nominalTime: 2.0 h
        bestTime: 3.716666 h
        nominalDistance: 80.0 km
      launch:
        nominalLaunch: 0.95999999
        flying: 84
        present: 91
      distance:
        sum: 9427.0 km
        flying: 84
        area: 52.9374
        nominalGoal: 0.2
        nominalDistance: 80.0 km
        minimumDistance: 5.0 km
        bestDistance: 159.3 km
    validity:
    - time: 1
      launch: 0.99468309
      distance: 1
      task: 0.99468309
    allocation:
    - goalRatio: 0.34523809
      weight:
        distance: 0.50519562
        leading: 8.659076e-2
        arrival: 6.185054e-2
        time: 0.34636306
      points:
        reach: 251.2
        effort: 251.2
        distance: 502.5
        leading: 86.1
        arrival: 61.5
        time: 344.5
      taskPoints: 994.0

.. raw:: latex

   \newpage

With all the information now at hand, we can tally points for the total
task score.

::

    score:
     - - Phil de Joux
        - total: 85.0
          breakdown:
            reach: 72.9
            effort: 12.1
            distance: 85.1
            leading: 0
            arrival: 0
            time: 0
          velocity:
            ss: 2012-01-14T03:33:34Z
            es: null
            distance: 46.2 km
            elapsed: null
            velocity: null
     ...
      - - Gerolf Heinrichs
        - total: 994.0
          breakdown:
            reach: 251.2
            effort: 251.2
            distance: 502.5
            leading: 86.1
            arrival: 61.5
            time: 344.5
          velocity:
            ss: 2012-01-14T03:31:13Z
            es: 2012-01-14T07:14:13Z
            distance: 159.3 km
            elapsed: 3.716666 h
            velocity: 42.8 km / h

.. [1]
   Reuse of inputs is not shown in
   Fig \ `[fig:flare-timing] <#fig:flare-timing>`__ as this would
   clutter the diagram too much. For instance ``*.kml`` and ``*.igc``
   track logs are also needed as inputs for the align step.

.. [2]
   As ``flare-timing`` is a work in progress, some further inputs will
   be needed as different kinds of task are tested, such as those with
   start gates, stopped tasks and those with penalties

.. [3]
   A zero leg distance indicates that the turnpoint was touched at one
   point only, the optimal route does not traverse the interior of the
   cylinder. The entry and exit waypoints are both shown but can be the
   same.

.. [4]
   Some pilots’ track logs will have initial values way off from the
   location of the device. I suspect that the GPS logger is remembering
   the position it had when last turned off, most likely at the end of
   yesterday’s flight, somewhere near where the pilot landed that day.
   Until the GPS receiver gets a satellite fix and can compute the
   current position the stale, last known, position gets logged. This
   means that a pilot may turn on their instrument inside the start
   circle but their tracklog will start outside of it.

.. [5]
   On a triangle course early fixes may cross goal.

.. [6]
   Currently ``flare-timing`` is just picking the fix inside the control
   zone.
