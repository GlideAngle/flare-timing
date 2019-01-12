Extracting Inputs
-----------------

In the ``*.fsdb`` FS keeps both inputs and outputs. We’re only
interested in a subset of the input data, just enough to do the
scoring [#]_;

Competition
    id, name, location, dates and UTC offset.

Nominal
    launch, goal, time, distance and minimal distance.

Task
    name and type of task, zones, speed section, start gates and pilots.

Zone
    name, latitude, longitude, altitude and radius.

Pilot
    name and either absentee status or track log file name.

Something to be aware of when parsing ``XML`` of ``*.fsdb`` is that
attributes may be missing and in that case we’ll have to infer the
defaults used by FS. This is done by looking at the source code of FS as
there is no schema for the ``XML`` that could also be used to set
default values.

.. code:: xml

    <Fs>
      <FsCompetition id="7592" name="2012 Hang Gliding Pre-World Forbes" location="Forbes, Australia"
          from="2012-01-05" to="2012-01-14" utc_offset="11">
        <!-- Nominals are set once for a competition but beware, they are repeated per task. -->
        <FsScoreFormula min_dist="5" nom_dist="80" nom_time="2" nom_goal="0.2" />
        <FsParticipants>
          <FsParticipant id="23" name="Gerolf Heinrichs" />
          <FsParticipant id="106" name="Adam Parer" />
        </FsParticipants>
          <!-- Flags on how to score are also set for the competition but pick them up from the task. -->
          <FsTask name="Day 8" tracklog_folder="Tracklogs\day 8">
            <FsScoreFormula use_distance_points="1" use_time_points="1" use_departure_points="0" use_leading_points="1" use_arrival_position_points="1" use_arrival_time_points="0" />
            <FsTaskDefinition ss="2" es="5" goal="LINE" groundstart="0">
              <!-- Not shown here but each FsTurnpoint has open and close attributes. -->
              <FsTurnpoint id="FORBES" lat="-33.36137" lon="147.93207" radius="100" />
              <FsTurnpoint id="FORBES" lat="-33.36137" lon="147.93207" radius="10000" />
              <FsTurnpoint id="MARSDE" lat="-33.75343" lon="147.52865" radius="5000" />
              <FsTurnpoint id="YARRAB" lat="-33.12908" lon="147.57323" radius="400" />
              <FsTurnpoint id="DAY8GO" lat="-33.361" lon="147.9315" radius="400" />
              <!-- This was an elapsed time task so no start gates. -->
            </FsTaskDefinition>
            <FsTaskState stop_time="2012-01-14T17:22:00+11:00" />
            <FsParticipants>
              <!-- An empty FsParticipant element is an absent pilot (ABS). -->
              <FsParticipant id="106" />
              <!-- With an empty FsFlightData element this is a pilot that did not fly (DNF). -->
              <FsParticipant id="80">
                <FsFlightData />
              </FsParticipant>
              <!-- A pilot with a tracklog (DF). -->
              <FsParticipant id="23">
                <FsFlightData tracklog_filename="Gerolf_Heinrichs.20120114-100859.6405.23.kml" />
              </FsParticipant>
              <!-- A pilot without tracklog to be awarded minimum distance (DF). -->
              <FsParticipant id="91">
                <FsFlightData tracklog_filename="" />
              </FsParticipant>
              <!-- A pilot without tracklog awarded a specified distance (DF). -->
              <FsParticipant id="85">
                <FsFlightData distance="95.030" tracklog_filename="" />
              </FsParticipant>
            </FsParticipants>
          </FsTask>
        </FsTasks>
      </FsCompetition>
    </Fs>

.. [#]
   As ``flare-timing`` is a work in progress, some further inputs will
   be needed as different kinds of task are tested, such as those with
   stopped tasks and those with penalties