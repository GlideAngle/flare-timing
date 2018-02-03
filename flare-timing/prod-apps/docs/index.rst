.. Flare Timing documentation master file, created by
   sphinx-quickstart on Sat Feb  3 10:15:05 2018.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Flare Timing
============

The `flare-timing <https://github.com/BlockScope/flare-timing>`_
implementation of GAP is a set of command line apps to be run in
sequence. The workings of each step and the final scores are written to
plain-text files. In these there’s enough detail and supporting evidence
for everything to be checked by hand.

Let’s now walk through the process of scoring with ``flare-timing``, see
Fig \ `[fig:flare-timing] <#fig:flare-timing>`__, and look at snippets
of the files it writes.

.. toctree::
   :maxdepth: 2
   :caption: Contents:

   extract-input
   task-length
   cross-zone
   tag-zone
   align-time
   discard-further
   mask-track
   land-out
   gap-point

Let’s now walk through the process of scoring with ``flare-timing``, see
Fig \ `[fig:flare-timing] <#fig:flare-timing>`__, and look at snippets
of the files it writes.

.. raw:: latex

   \centering

Starting with an ``*.fsdb`` comp and related ``*.igc`` or ``*.kml``
track logs, scoring proceeds in steps  [1]_;

#. Extract the inputs with
   `extract-input <https://github.com/BlockScope/flare-timing/tree/master/flare-timing/prod-apps/extract-input>`__.

#. Trace the route of the shortest path to fly a task with
   `task-length <https://github.com/BlockScope/flare-timing/tree/master/flare-timing/prod-apps/task-length>`__.

#. Find pairs of fixes crossing over zones with
   `cross-zone <https://github.com/BlockScope/flare-timing/tree/master/flare-timing/prod-apps/cross-zone>`__.

#. Interpolate between crossing fixes for the time and place where a
   track tags a zone with
   `tag-zone <https://github.com/BlockScope/flare-timing/tree/master/flare-timing/prod-apps/tag-zone>`__.

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



.. [1]
   Reuse of inputs is not shown in
   Fig \ `[fig:flare-timing] <#fig:flare-timing>`__ as this would
   clutter the diagram too much. For instance ``*.kml`` and ``*.igc``
   track logs are also needed as inputs for the align step.


Indices and tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`

