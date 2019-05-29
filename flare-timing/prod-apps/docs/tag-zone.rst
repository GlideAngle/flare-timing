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
