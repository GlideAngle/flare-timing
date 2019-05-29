Interpolating Zone Taggings
---------------------------

Between the pair of fixes straddling a control zone, we need to
interpolate the point at which the pilot is most likely to have crossed
and the time of this tagging of the turnpoint [#]_.

.. highlight:: yaml
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
   Currently ``flare-timing`` is just picking the fix inside the control
   zone.
