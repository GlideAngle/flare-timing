Assessing Difficulty
--------------------

We consider only pilots landing out in 100m chunks of the task. How many
came down in a chunk and how many are downward bound is shown. For each
chunk with a landing, we have its relative difficulty. Taking the sum of
these under a moving window we have difficulty fractions to award to
pilots.

.. highlight:: yaml
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
