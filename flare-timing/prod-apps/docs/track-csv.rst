Track Log Tables
================

The next steps all work to produce ``*.csv`` files that share a common set of
headers so that they can be compared easily. These headers are:

.. highlight:: text
::

    fixIdx,time,lat,lng,alt,tickLead,tickRace,zoneIdx,legIdx,togo,area

The steps for processing track logs proceeds as follows:

#. Unpack track logs with
   `unpack-track <https://github.com/BlockScope/flare-timing/tree/master/flare-timing/prod-apps/unpack-track>`__.

#. Index fixes from the time of first crossing with
   `align-time <https://github.com/BlockScope/flare-timing/tree/master/flare-timing/prod-apps/align-time>`__.

#. Discard fixes that get further from goal and note leading area with
   `discard-further <https://github.com/BlockScope/flare-timing/tree/master/flare-timing/prod-apps/discard-further>`__.

#. Discard fixes after first applying any distance for altitude bonus
   with
   `peg-then-discard <https://github.com/BlockScope/flare-timing/tree/master/flare-timing/prod-apps/peg-then-discard>`__.

#. Discard fixes before applying any distance for altitude bonus
   `discard-then-peg <https://github.com/BlockScope/flare-timing/tree/master/flare-timing/prod-apps/discard-then-peg>`__.

Unpacking Tracks
----------------

The ``*.igc`` file format has a header record for the date with fix records
offset in seconds from this.

::

    HFDTE0301181
         ^^^^^^^
         on 2018-01-03
    ...
    B0405473321383S14756040EA0024800227
     ^^^^^^
     at 04:05:47

The ``*.kml`` format sometimes used in competitions is similar with a timestamp
for the first point and then seconds of offset from that for each subsequent
fix.

.. highlight:: xml
::

    <Placemark>
      <Metadata src="GpsDump" v="4.66" type="track">
        <FsInfo time_of_first_point="2012-01-14T02:12:55Z"
                civl_pilot_id="21437" comp_pilot_id="40"
                instrument="6030 SN06451 SW3.30"
                downloaded="2012-01-14T08:22:21Z"
                hash="61168B84FE0DAC55F3D65EFBA888B08F72834DDF">
          <SecondsFromTimeOfFirstPoint>
    0 2 4 ...
          </SecondsFromTimeOfFirstPoint>
          <PressureAltitude>
    239 240 240 ..
          </PressureAltitude>
        </FsInfo>
      </Metadata>
      <name>Tracklog</name>
      <LineString>
        <altitudeMode>absolute</altitudeMode>
        <coordinates>
    147.932050,-33.361600,237 147.932050,-33.361600,238 147.932050,-33.361600,238 ...
        </coordinates>
      </LineString>
    </Placemark>

The ``unpack-track`` step sets values for these fields:

.. highlight:: text
::

    fixIdx,time,lat,lng,alt

Aligning Tracks by Elapsed Time
-------------------------------

We align the tracks in time elapsed from the first start and work out the
distance flown for each fix. All fields are set except area so by this stage we
know which leg of the course a fix is associated with, the ``legIdx``, and
which fix it is within a leg, the ``zoneIdx``. The ``tickLead`` and
``tickRace`` are aligning the track logs over all competitors flying a task:

::

    fixIdx,time,lat,lng,alt,tickLead,tickRace,zoneIdx,legIdx,togo


Discarding Fixes further from Goal
----------------------------------

We discard any fixes that get further from goal so that ``togo`` always
decreases and work out the leading area for each increment of distance. This is
the track log data used for time and leading points.

::

    fixIdx,time,lat,lng,alt,tickLead,tickRace,zoneIdx,legIdx,togo,area

When no pilot makes it through the speed section no time points are awarded.
The last row holds the minimum ``togo`` value.  When subtracted from the task
length this gives a pilot's distance flown or reach. The maximum reach, the
task best distance, is compared to the competition's nominal distance to
determine the time validity for tasks where no pilot makes it through the speed
section.

Peg and then Discard
--------------------

When circling in thermals altitude gains are often uneven. Sharing a thermal
with another pilot we'll see them go up and down relative to us around the
turn. The GAP rules say that an altitude bonus distance is calculated for each
fix in a pilots track log. From this it would be right to apply the bonus
before discarding fixes.
::

    fixIdx,time,lat,lng,alt,tickLead,tickRace,zoneIdx,legIdx,togo,area

Discard and then Peg
--------------------

What if the altitude bonus was applied after discarding fixes further from
goal? We could potentially end up with negative slivers of leading area. In any
case altitude bonus distance is ignored for the purpose of calculating leading
area.

::

    fixIdx,time,lat,lng,alt,tickLead,tickRace,zoneIdx,legIdx,togo,area
                                                                  ^^^^
                                                                  sometimes negative
