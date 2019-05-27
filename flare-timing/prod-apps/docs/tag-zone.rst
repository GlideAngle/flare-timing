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

.. [#]
   Currently ``flare-timing`` is just picking the fix inside the control
   zone.
