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

