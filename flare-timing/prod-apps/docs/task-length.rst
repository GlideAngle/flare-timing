Tracing an Optimal Route
------------------------

To find the best route ``flare-timing`` constructs a graph and finds the
shortest path connecting the nodes. It puts nodes on turnpoint cylinder
arc boundaries and uses the haversine distance as the cost of connecting
nodes in the network. It would be expensive to construct and evaluate a
large network with the accuracy required so in an iterative process, as
the arc of the circle is shortened, getting closer to the optimal
crossing point, the density of nodes is increased. All happening on the
FAI sphere, this is the edge to edge optimal route. Routes are shown
with waypoints, segment distances  [3]_ and total distance.

::

    taskRoutes:
      edgeToEdge:
        distance: 159.373683
        legs:
        - 10.078208
        - 42.525217
        - 0
        - 64.949832
        - 41.820427
        legsSum:
        - 10.078208
        - 52.603424
        - 52.603424
        - 117.553256
        - 159.373683
        waypoints:
        - lat: -33.36047067
          lng: 147.93206999
        - lat: -33.43411056
          lng: 147.86878018
        - lat: -33.7159199
          lng: 147.55846831
        - lat: -33.7159199
          lng: 147.55846831
        - lat: -33.13199024
          lng: 147.57575486
        - lat: -33.35857718
          lng: 147.93468357

The naive way to measure task length would be to just connect the
centers of each control zone. This is the point to point distance.

::

    taskRoutes:
      pointToPoint:
        distance: 169.10714
        legs:
        - 57.427511
        - 69.547668
        - 42.131961
        legsSum:
        - 57.427511
        - 126.975179
        - 169.10714
        waypoints:
        - lat: -33.36137
          lng: 147.93207
        - lat: -33.75343
          lng: 147.52864998
        - lat: -33.12908
          lng: 147.57322998
        - lat: -33.36099999
          lng: 147.93149998

.. raw:: latex

   \newpage

Knowing that FS uses a plane to work out the shortest route in two
dimensions, on the the Universal Transverse Mercator projection, we can
also do that with our graph algorithm. We end up with waypoints, optimal
on the plane but possibly sub-optimal on the sphere.

::

    taskRoutes:
      projection:
        spherical:
          distance: 159.373683
          legs:
          - 10.078208
          - 42.525217
          - 0
          - 64.949832
          - 41.820427
          legsSum:
          - 10.078208
          - 52.603424
          - 52.603424
          - 117.553256
          - 159.373683
          waypoints:
          - lat: -33.36047067
            lng: 147.93206999
          - lat: -33.43411056
            lng: 147.86878018
          - lat: -33.7159199
            lng: 147.55846831
          - lat: -33.7159199
            lng: 147.55846831
          - lat: -33.13199024
            lng: 147.57575486
          - lat: -33.35857718
            lng: 147.93468357

::

    taskRoutes:
      projection:
        planar:
          distance: 159.144781
          legs:
          - 10.065441
          - 42.4942
          - 0
          - 64.761082
          - 41.820427
          legsSum:
          - 10.065441
          - 52.559642
          - 52.559642
          - 117.320723
          - 159.14115
          mappedPoints:
          - easting: 586715.834
            northing: 6308362.198
          - easting: 580759.282
            northing: 6300248.47
          - easting: 551744.701
            northing: 6269201.551
          - easting: 551744.701
            northing: 6269201.551
          - easting: 553704.761
            northing: 6333932.964
          - easting: 586960.882
            northing: 6308569.955
          mappedZones:
          - latZone: H
            lngZone: 55

.. raw:: latex

   \newpage

.. [3]
   A zero leg distance indicates that the turnpoint was touched at one
   point only, the optimal route does not traverse the interior of the
   cylinder. The entry and exit waypoints are both shown but can be the
   same.

