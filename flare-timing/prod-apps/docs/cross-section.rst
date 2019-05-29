Crossing Zones and Sectioning Tracks
====================================

For each pilot track, we need to work out which zones have been crossed and
then select from these crossings the tagging of each zone. Taking into account
the timing restriction of the task we can section the times and fixes selecting
the subset of these that will be scored.

Finding Zone Crossings
----------------------

Before we can determine if any zones have been crossed we’ll have to
decide how to tell which parts of a track log are flown and which are
walked or driven in the retrieve car, possibly even back to goal. [#]_

To work out when a pilot is flying, select the longest run of fixes that
are not the same allowing for some stickiness when the GPS loses signal.
For example we might consider within ± 1m altitude or within ±
1/10,000th of a degree of latitude or longitude to be in the same
location and not likely recorded during flight.

.. highlight:: yaml
::

    flying:
      - - - '23'
          - Gerolf Heinrichs
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
sequence of crossings ordered in time that fits the task [#]_.

::

    crossing:
      - - - '23'
          - Gerolf Heinrichs
        - zonesCrossSelected:
          - crossingPair: ...
          - crossingPair: ...
          - crossingPair: ...
          - crossingPair: ...
          - crossingPair:
            - fix: 4714
              time: 2012-01-14T07:14:21Z
              lat: -33.360479
              lng: 147.931166
              alt: 377
            - fix: 4715
              time: 2012-01-14T07:14:25Z
              lat: -33.361015
              lng: 147.931573
              alt: 372
            inZone:
            - false
            - true
          zonesCrossNominees: ...

Interpolating Zone Taggings
---------------------------

Between the pair of fixes straddling a control zone, we need to
interpolate the point at which the pilot is most likely to have crossed
and the time of this tagging of the turnpoint.

.. highlight:: yaml
::

    tagging:
      - - - '23'
          - Gerolf Heinrichs
        - zonesTag:
          - inter: ...
          - inter: ...
          - inter: ...
          - inter: ...
          - inter:
              fixFrac: 4714.903706113713
              time: 2012-01-14T07:14:24.614824454852Z
              lat: -33.36096338
              lng: 147.93153381
              alt: 372.48
            cross:
              crossingPair:
              - fix: 4714
                time: 2012-01-14T07:14:21Z
                lat: -33.360479
                lng: 147.931166
                alt: 377
              - fix: 4715
                time: 2012-01-14T07:14:25Z
                lat: -33.361015
                lng: 147.931573
                alt: 372
              inZone:
              - false
              - true

Sorting the list of tagging times, we can show the first and last times,
the count of taggings and the pilots.

::

    timing:
    - zonesSum:
      - 74
      - 81
      - 73
      - 54
      - 27
      zonesFirst:
      - 2012-01-14T02:00:38.517193949596Z
      - 2012-01-14T02:43:11.03560045651Z
      - 2012-01-14T04:26:05.006295836437Z
      - 2012-01-14T06:21:02.137866989328Z
      - 2012-01-14T07:14:24.614824454852Z
      zonesLast:
      - 2012-01-14T03:10:04.202693584608Z
      - 2012-01-14T04:19:09.263939298134Z
      - 2012-01-14T07:16:54.579375206841Z
      - 2012-01-14T08:06:40.357924724524Z
      - 2012-01-14T08:12:57.60359403562Z
      zonesRankTime:
      - - 2012-01-14T02:00:38.517193949596Z
        - ...
      - - 2012-01-14T02:43:11.03560045651Z
        - ...
      - - 2012-01-14T04:26:05.006295836437Z
        - ...
      - - 2012-01-14T06:21:02.137866989328Z
        - ...
      - - 2012-01-14T07:14:24.614824454852Z
        - 2012-01-14T07:31:07.089658199088Z
        - 2012-01-14T07:35:18.31771989944Z
        - ...
      zonesRankPilot:
      - - - '51'
          - Rob In 't Groen
        - ...
      - - - '88'
          - Martin Sielaf
        - ...
      - - - '66'
          - Jonas Lobitz
        - ...
      - - - '23'
          - Gerolf Heinrichs
        - ...
      - - - '23'
          - Gerolf Heinrichs
        - - '100'
          - Attila Bertok
        - - '66'
          - Jonas Lobitz
        - - ...
      lastLanding: 2012-01-14T08:41:04Z

.. [#]
   Some pilots’ track logs will have initial values way off from the
   location of the device. I suspect that the GPS logger is remembering
   the position it had when last turned off, most likely at the end of
   yesterday’s flight, somewhere near where the pilot landed that day.
   Until the GPS receiver gets a satellite fix and can compute the
   current position the stale, last known, position gets logged. This
   means that a pilot may turn on their instrument inside the start
   circle but their tracklog will start outside of it.

.. [#]
   On a triangle course early fixes may cross goal.
