Masking Task over Track
-----------------------

Taking what we now know about the tracks and the task, we have times,
distances and fractions we’ll need later for the points. So far we have
leading, arrival and speed fractions but we still need to look at where
pilots landed to have the distance fractions.

.. highlight:: yaml
::

    pilotsAtEss:
    ...
    - 29
    raceTime:
    ...
    - openTask: 2012-01-14T01:00:00Z
      closeTask: 2012-01-14T09:00:00Z
      firstStart: 2012-01-14T02:43:11.03560045651Z
      firstLead: 2012-01-14T02:43:11.03560045651Z
      lastArrival: 2012-01-14T08:12:57.60359403562Z
      leadAllDown: 21472.964
      leadArrival: 19786.568
      leadClose: 22608.964
      openClose: 28800
      tickClose: 22608.964
    ssBestTime:
    ...
    - 3.719213 h
    gsBestTime:
    ...
    - 3.719213 h
    taskDistance:
    ...
    - 158.864751 km
    taskSpeedDistance:
    ...
    - 148.964751 km
    bestDistance:
    ...
    - 158.864751 km
    sumDistance:
    ...
    - 9486.622084 km
    leadAreaToCoef:
    ...
    - 0.00000002503574 km^-2 s^-1
    leadCoefMin:
    ...
    - 4.12428993
    leadRank:
    ...
    - - - - '23'
          - Gerolf Heinrichs
        - area: 164736078.4532 km^2 s
          coef: 4.12428993
          frac: 1
      - - - '66'
          - Jonas Lobitz
        - area: 166917443.8758 km^2 s
          coef: 4.17890204
          frac: 0.91024137
      - - - '51'
          - Rob In 't Groen
        - area: 170139661.7053 km^2 s
          coef: 4.25957265
          frac: 0.83567191
     ...
    arrivalRank:
    ...
    - - - - '23'
          - Gerolf Heinrichs
        - rank: '1'
          frac: 1
      - - - '100'
          - Attila Bertok
        - rank: '2'
          frac: 0.92666251
      - - - '66'
          - Jonas Lobitz
        - rank: '3'
          frac: 0.85799451
      ...
    flownMean:
    ...
    - 117.935977 km
    flownStdDev:
    ...
    - 49.749145 km
    reachMean:
    ...
    - 117.935977 km
    reachStdDev:
    ...
    - 49.749145 km
    reachRank:
    ...
    - - - - '23'
          - Gerolf Heinrichs
        - frac: 1
          reach: 158.864751 km
      - - - '100'
          - Attila Bertok
        - frac: 1
          reach: 158.864751 km
      - - - '66'
          - Jonas Lobitz
        - frac: 1
          reach: 158.864751 km
      ...
    ssSpeed: ...
    gsSpeed:
    - - - - '23'
          - Gerolf Heinrichs
        - time: 3.719213 h
          frac: 1
      - - - '83'
          - Peter Dall
        - time: 3.924142 h
          frac: 0.77565394
      - - - '41'
          - Curt Warren
        - time: 3.947778 h
          frac: 0.75871917
      ...

For those landing out, how close or nigh were they to goal and where did
they land?

::

    nigh:
    ...
    - - - '40'
          - Phil de Joux
        - togo:
            distance: 112.781482 km
            flipSum:
            - 112.781482 km
            - 106.020325 km
            - 41.776873 km
            legs:
            - 6.761157 km
            - 64.243452 km
            - 41.776873 km
            legsSum:
            - 6.761157 km
            - 71.004609 km
            - 112.781482 km
            waypoints:
            - lat: -33.651233
              lng: 147.560767
            - lat: -33.70932748
              lng: 147.53919553
            - lat: -33.13234684
              lng: 147.57502845
            - lat: -33.36226617
              lng: 147.93033075
          made: 46.083751 km
      - - - '53'
          - Hadewych van Kempen
        - togo:
            distance: 123.188199 km
            flipSum:
            - 123.188199 km
            - 106.093143 km
            - 41.776873 km
            legs:
            - 17.095056 km
            - 64.316270 km
            - 41.776873 km
            legsSum:
            - 17.095056 km
            - 81.411326 km
            - 123.188199 km
            waypoints:
            - lat: -33.577367
              lng: 147.6364
            - lat: -33.71015129
              lng: 147.54332275
            - lat: -33.13234684
              lng: 147.57502845
            - lat: -33.36226617
              lng: 147.93033075
          made: 35.676751 km
    ...

::

    land:
    - - - - '41'
          - Curt Warren
        - togo: 0.000000 km
          made: 158.864751 km
      - - - '95'
          - Anton Struganov
        - togo: 0.000000 km
          made: 158.864751 km
      - - - '30'
          - Fredy Bircher
        - togo: 0.827092 km
          made: 158.037659 km
      ...
      - - - '40'
          - Phil de Joux
        - togo: 112.823874 km
          made: 46.040877 km
      - - - '53'
          - Hadewych van Kempen
        - togo: 123.477146 km
          made: 35.387605 km
      ...
