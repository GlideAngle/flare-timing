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
          - crossingPair:
            - fix: 294
              time: 2012-01-14T02:19:41Z
              lat: -33.360586
              lng: 147.931616
              alt: 283
            - fix: 295
              time: 2012-01-14T02:19:45Z
              lat: -33.360243
              lng: 147.931187
              alt: 286
            inZone:
            - true
            - false
          - crossingPair:
            - fix: 1367
              time: 2012-01-14T03:31:13Z
              lat: -33.414724
              lng: 147.846
              alt: 2317
            - fix: 1368
              time: 2012-01-14T03:31:17Z
              lat: -33.415217
              lng: 147.845399
              alt: 2309
            inZone:
            - true
            - false
          - crossingPair:
            - fix: 2207
              time: 2012-01-14T04:27:13Z
              lat: -33.707943
              lng: 147.530529
              alt: 1303
            - fix: 2208
              time: 2012-01-14T04:27:17Z
              lat: -33.708522
              lng: 147.530401
              alt: 1302
            inZone:
            - false
            - true
          - crossingPair:
            - fix: 3914
              time: 2012-01-14T06:21:01Z
              lat: -33.132856
              lng: 147.572737
              alt: 2111
            - fix: 3915
              time: 2012-01-14T06:21:05Z
              lat: -33.132169
              lng: 147.573016
              alt: 2105
            inZone:
            - false
            - true
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
