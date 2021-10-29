module MaskLeadOptions (description) where

import Text.RawString.QQ (r)
import Flight.Cmd.Options (Description(..))

description :: Description
description = Description [r|
By masking the track logs with the zones, works out when pilots were leading.

Where 'c' is the comp name, 'p' is the pilot name, '.' is the folder with
competition inputs and k is a folder path specified in the inputs for
tracklogs, one per task;
    Reads  ./c.comp-input.yaml
    Reads  ./k/p.kml
    Reads  ./c.tag-zone.yaml
    Reads  ./c.mask-arrival.yaml
    Writes ./flare-timing/discard-further/task-n/p.csv
    Writes ./c.mask-track.yaml

If a list of tasks are supplied then those tasks alone are processed, otherwise
all tasks are processed. The same thing goes if a list of pilots is supplied or
not.
|]
