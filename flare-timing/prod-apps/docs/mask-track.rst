Masking Task over Track
-----------------------

Taking what we now know about the tracks and the task, we have times,
distances and fractions we’ll need later for the points. So far we have
leading, arrival and speed fractions but we still need to look at where
pilots landed to have the distance fractions.

.. highlight:: yaml
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
