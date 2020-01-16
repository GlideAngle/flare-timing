Scoring Steps
-------------

Let’s now walk through the process of scoring with ``flare-timing``.  Starting
from an ``*.fsdb`` competition with related ``*.igc`` or ``*.kml`` track logs,
scoring proceeds in steps  [#]_;

#. Extract the inputs with
   `extract-input <https://github.com/BlockScope/flare-timing/tree/master/flare-timing/prod-apps/extract-input>`__.

#. Trace the route of the shortest path to fly a task with
   `task-length <https://github.com/BlockScope/flare-timing/tree/master/flare-timing/prod-apps/task-length>`__.

#. Find pairs of fixes crossing over zones with
   `cross-zone <https://github.com/BlockScope/flare-timing/tree/master/flare-timing/prod-apps/cross-zone>`__.

#. Interpolate between crossing fixes with
   `tag-zone <https://github.com/BlockScope/flare-timing/tree/master/flare-timing/prod-apps/tag-zone>`__.

#. Unpack track logs to a flat list of time with latitude and longitude with
   `unpack-track
   <https://github.com/BlockScope/flare-timing/tree/master/flare-timing/prod-apps/unpack-track>`__
   . This data supplies the pilot tracks that can be downloaded and shown on
   the map and is used to find the scored track indices for stopped tasks.

#. Peg out the time range of each track log that will be scored with
   `peg-frame <https://github.com/BlockScope/flare-timing/tree/master/flare-timing/prod-apps/peg-frame>`__.

#. Index fixes from the time of first crossing with
   `align-time <https://github.com/BlockScope/flare-timing/tree/master/flare-timing/prod-apps/align-time>`__.

#. Discard fixes that get further from goal and note leading area with
   `discard-further <https://github.com/BlockScope/flare-timing/tree/master/flare-timing/prod-apps/discard-further>`__.

#. Mask a task over its tracklogs with
   `mask-track <https://github.com/BlockScope/flare-timing/tree/master/flare-timing/prod-apps/mask-track>`__.

#. Group and count land outs with
   `land-out <https://github.com/BlockScope/flare-timing/tree/master/flare-timing/prod-apps/land-out>`__.

#. Score the competition with
   `gap-point <https://github.com/BlockScope/flare-timing/tree/master/flare-timing/prod-apps/gap-point>`__.

If hosting the web app then the following two steps are needed too:

#. Extract the scores and some of the workings of FS with `fs-score
   <https://github.com/BlockScope/flare-timing/tree/master/flare-timing/prod-apps/fs-score>`__.
   These values and differences are shown as extra columns in tables
   side-by-side with ``flare-timing`` values. This is a quick way to spot
   discrepancies between the two GAP scoring implementations.

.. [#]
   In this list, any step can use the inputs or outputs from a previous step.
   For instance ``*.kml`` and ``*.igc`` track logs are needed as inputs for the
   cross-zone, align-time and unpack-track steps. The app of each step logs the
   inputs they're using.
