Assessing Difficulty
--------------------

We consider only pilots landing out in 100m chunks of the task. How many
came down in a chunk and how many are downward bound is shown. For each
chunk with a landing, we have its relative difficulty. Taking the sum of
these under a moving window we have difficulty fractions to award to
pilots.

.. highlight:: yaml
::

    minDistance: 5.000 km
    bestDistance:
    ...
    - 158.865 km
    landout:
    ...
    - 63
    lookahead:
    ...
    - 76
    sumOfDifficulty:
    ...
    - 266
    difficulty:
    ...
    - - chunk: 0
        startChunk: 5.0 km
        endChunk: 5.0 km
        endAhead: 12.6 km
        down: 3
        downs:
        - 5.000 km
        - 5.000 km
        - 5.000 km
        downers:
        - - '32'
          - Kathryn O'Riordan
        - - '64'
          - Gustavo Carvalho
        - - '28'
          - Evgeniya Laritskaya
        downward: 3
        rel: 5.6391e-3
        frac: 5.6391e-3
        ...
      - chunk: 304
        startChunk: 35.3 km
        endChunk: 35.4 km
        endAhead: 43.0 km
        down: 1
        downs:
        - 35.388 km
        downers:
        - - '53'
          - Hadewych van Kempen
        downward: 1
        rel: 1.8797e-3
        frac: 4.887218e-2
      - chunk: 411
        startChunk: 46.0 km
        endChunk: 46.1 km
        endAhead: 53.7 km
        down: 1
        downs:
        - 46.041 km
        downers:
        - - '40'
          - Phil de Joux
        downward: 5
        rel: 9.3985e-3
        frac: 5.827068e-2
        ...
