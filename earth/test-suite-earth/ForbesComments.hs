module ForbesComments where

{-
NOTE: The task distances show below are taken from the competition *.fsdb file
at the path /Fs/FsCompetition/FsTasks/FsTask/FsTaskScoreParams/FsTaskDistToTp.
The first distance is not 9.9 kms, 10 kms - 100m.

Some flight instruments use WGS84 and others use the FAI spheriod. To
accomodate this, there is a tolerance of either 0.01% or 0.5% used, depending
on the competition. For category 1 events since 2015-01-01 it is 0.01%.
Category 2 events can elect to use the wider margin. This tolerance is used for
working out if tracks reach control zones.

The optimised route is worked out by FS in 2D space from a UTM projection. This
accounts for the discrepency with errors coming from choosing wrong waypoints
for the optimal route and from the conversion of these points back to the FAI
sphere.

TODO: Find out why the first distance is 9.882 and not 9.9 km.
<FsTaskDistToTp tp_no="1" distance="0" />
<FsTaskDistToTp tp_no="2" distance="9.882" />
<FsTaskDistToTp tp_no="3" distance="54.254" />
<FsTaskDistToTp tp_no="4" distance="112.779" />
<FsTaskDistToTp tp_no="5" distance="133.357" />

The unit tests here are not comparing to distances of FS but to point to point
distances worked out on the FAI sphere.

SEE: http://www.stevemorse.org/nearest/distancebatch.html
SEE: http://www.movable-type.co.uk/scripts/latlong-vincenty.html
SEE: http://www.anycalculator.com/longitude.htm
SEE: http://andrew.hedges.name/experiments/haversine/

-33.36137, 147.93207, -33.85373, 147.94195, -33.4397, 148.34533, -33.61965, 148.4099

-33.36137, 147.93207, -33.85373, 147.94195
-33.85373, 147.94195, -33.4397, 148.34533
-33.4397, 148.34533, -33.61965, 148.4099

NOTE: Point to point distances using Haversine method.
=> 
54.76
59.28
20.89

54.76 + 59.28 + 20.89
=> 134.93

134.93 - 10 - 0.4
=> 124.53

NOTE: Point to point distances using Vincenty method.
=> 
54.62
59.24
20.84

54.62 + 59.24 + 20.84
=> 134.7

134.7 - 10 - 0.4
=> 124.30

- sphericalPointToPoint:
    distance: 134.917675
    legs:
    - 54.755578
    - 59.276627
    - 20.88547
    legsSum:
    - 54.755578
    - 114.032205
    - 134.917675
    waypoints:
    - lat: -33.36137
      lng: 147.93207
    - lat: -33.85372998
      lng: 147.94194999
    - lat: -33.4397
      lng: 148.34532999
    - lat: -33.61965
      lng: 148.40989999
-}

{-
-33.36137, 147.93207, -32.90223, 147.98492, -32.9536, 147.55457, -33.12592, 147.91043

-33.36137, 147.93207, -32.90223, 147.98492
-32.90223, 147.98492, -32.9536, 147.55457
-32.9536, 147.55457, -33.12592, 147.91043

NOTE: Point to point distances using Haversine method.
=>
51.29
40.57
38.31

51.29 + 40.57 + 38.31
=> 130.17

130.17 - 5 - 0.4
=> 124.77

NOTE: Point to point distances using Vincenty method.
=>
51.16
40.65
38.34

51.16 + 40.65 + 38.34
=> 130.15

130.15 - 5 - 0.4
=> 124.75

- sphericalPointToPoint:
    distance: 130.167733
    legs:
    - 51.290669
    - 40.569544
    - 38.30752
    legsSum:
    - 51.290669
    - 91.860213
    - 130.167733
    waypoints:
    - lat: -33.36137
      lng: 147.93207
    - lat: -32.90223
      lng: 147.98491999
    - lat: -32.9536
      lng: 147.55457
    - lat: -33.12592
      lng: 147.91042999
-}

{-
-33.36137, 147.93207, -34.02107, 148.2233, -34.11795, 148.5013, -34.82197, 148.66543

-33.36137, 147.93207, -34.02107, 148.2233
-34.02107, 148.2233, -34.11795, 148.5013
-34.11795, 148.5013, -34.82197, 148.66543

NOTE: Point to point distances using Haversine method.
=>
78.15
27.78
79.72

78.15 + 27.78 + 79.72
=> 185.65

185.65 - 25 - 0.4
=> 160.25

NOTE: Point to point distances using Vincenty method.
=>
77.99
27.82
79.54

77.99 + 27.82 + 79.54
=> 185.35

185.35 - 25 - 0.4
=> 159.95

- sphericalPointToPoint:
    distance: 185.643415
    legs:
    - 78.147093
    - 27.780099
    - 79.716223
    legsSum:
    - 78.147093
    - 105.927192
    - 185.643415
    waypoints:
    - lat: -33.36137
      lng: 147.93207
    - lat: -34.02107
      lng: 148.22329998
    - lat: -34.11795
      lng: 148.50129999
    - lat: -34.82197
      lng: 148.66542999
-}

{-
-33.36137, 147.93207, -32.90223, 147.98492, -32.46363, 148.989

-33.36137, 147.93207, -32.90223, 147.98492
-32.90223, 147.98492, -32.46363, 148.989

NOTE: Point to point distances using Haversine method.
=>
51.29
105.9

51.29 + 105.9
=> 157.19

157.19 - 15 - 0.4
=> 141.79

NOTE: Point to point distances using Vincenty method.
=>
51.16
106

51.16 + 106
=> 157.16

157.16 - 15 - 0.4
=> 141.76

- sphericalPointToPoint:
    distance: 157.16322
    legs:
    - 51.290669
    - 105.87255
    legsSum:
    - 51.290669
    - 157.16322
    waypoints:
    - lat: -33.36137
      lng: 147.93207
    - lat: -32.90223
      lng: 147.98491999
    - lat: -32.46363
      lng: 148.989
-}

{-
-33.36137, 147.93207, -32.56608, 148.22657, -32.0164, 149.43363

-33.36137, 147.93207, -32.56608, 148.22657
-32.56608, 148.22657, -32.0164, 149.43363

NOTE: Point to point distances using Haversine method.
=>
92.6
128.9

92.6 + 128.9
=> 221.5

221.5 - 15 - 0.4
=> 206.1

NOTE: Point to point distances using Vincenty method.
=>
92.4
129

92.4 + 129
=> 221.4

221.4 - 15 - 0.4
=> 206.0

- sphericalPointToPoint:
    distance: 221.477524
    legs:
    - 92.601904
    - 128.87562
    legsSum:
    - 92.601904
    - 221.477524
    waypoints:
    - lat: -33.36137
      lng: 147.93207
    - lat: -32.56607998
      lng: 148.22657
    - lat: -32.01639998
      lng: 149.43362998
-}

{-
-33.36137, 147.93207, -32.19498, 147.76218, -31.69323, 148.29623

-33.36137, 147.93207, -32.19498, 147.76218
-32.19498, 147.76218, -31.69323, 148.29623

NOTE: Point to point distances using Haversine method.
=>
130.7
75.18

130.7 + 75.18
=> 205.88 

205.88 - 15 - 0.4
=> 190.48

NOTE: Point to point distances using Vincenty method.
=>
130.3
75.13

130.3 + 75.13
=> 205.43

205.43 - 15 - 0.4
=> 190.03

- sphericalPointToPoint:
    distance: 205.844959
    legs:
    - 130.665489
    - 75.17947
    legsSum:
    - 130.665489
    - 205.844959
    waypoints:
    - lat: -33.36137
      lng: 147.93207
    - lat: -32.19498
      lng: 147.76218
    - lat: -31.69322998
      lng: 148.29623
-}

{-
-33.36137, 147.93207, -32.9536, 147.55457, -32.76052, 148.64958, -32.93585, 148.74947

-33.36137, 147.93207, -32.9536, 147.55457
-32.9536, 147.55457, -32.76052, 148.64958
-32.76052, 148.64958, -32.93585, 148.74947

NOTE: Point to point distances using Haversine method.
=>
57.37
104.5
21.61

57.37 + 104.5 + 21.61
=> 183.48

183.48 - 10 - 0.4
=> 173.08

NOTE: Point to point distances using Vincenty method.
=>
57.32
104.7
21.58

57.32 + 104.7 + 21.58
=> 183.60

183.60 - 10 - 0.4
=> 173.2

- sphericalPointToPoint:
    distance: 183.488931
    legs:
    - 57.365312
    - 104.509732
    - 21.613886
    legsSum:
    - 57.365312
    - 161.875045
    - 183.488931
    waypoints:
    - lat: -33.36137
      lng: 147.93207
    - lat: -32.9536
      lng: 147.55457
    - lat: -32.76051998
      lng: 148.64957999
    - lat: -32.93585
      lng: 148.74947
-}

{-
-33.36137, 147.93207, -33.75343, 147.52865, -33.12908, 147.57323, -33.361, 147.9315

-33.36137, 147.93207, -33.75343, 147.52865
-33.75343, 147.52865, -33.12908, 147.57323
-33.12908, 147.57323, -33.361, 147.9315

NOTE: Point to point distances using Haversine method.
=>
57.43
69.55
42.13

57.43 + 69.55 + 42.13
=> 169.11

169.11 - 10 - 0.4
=> 158.71

NOTE: Point to point distances using Vincenty method.
=>
57.4
69.37
42.15

57.4 + 69.37 + 42.15
=> 168.92

169.92 - 10 - 0.4
=> 159.52

- sphericalPointToPoint:
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
-}
