module AlignTimeOptions (description) where

import Text.RawString.QQ (r)
import Flight.Cmd.Options (Description(..))

description :: Description
description = Description [r|
From competition inputs '.comp-input.yaml', relative track logs '.kml' and
tagged zones '.tag-zone.yaml', writes each fix to a single '.csv' per-pilot
per-task, grouped into a single folder per-task.

Using the tagged zones to find the first crossing of a pilot into the speed
section, three columns are added to the CSV;
    leg        The leg of the speed section of the task
    tick       The time in seconds from the first crossing
    distance   The distance to goal

Where 'c' is the comp name, 'p' is the pilot name, '.' is the folder with
competition inputs and k is a folder path specified in the inputs for
tracklogs, one per task;
    Reads  ./c.comp-input.yaml
    Reads  ./c.tag-zone.yaml
    Reads  ./k/p.kml
    Writes ./flare-timing/align-time/task-n/p.csv

If a list of tasks are supplied then those tasks alone are processed, otherwise
all tasks are processed. The same thing goes if a list of pilots is supplied or
not.
|]