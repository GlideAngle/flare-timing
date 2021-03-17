module DiscardFurtherStopOptions (description) where

import Text.RawString.QQ (r)
import Flight.Cmd.Options (Description(..))

description :: Description
description = Description [r|
From competition inputs '.comp-input.yaml', finds the aligned time '.csv' files
for each pilot. Keeping only tick and distance columns, initial zero distance
rows are dropped and rows that get further from goal are also dropped.

Where 'p' is the pilot name and '.' is the folder with competition inputs;
    Reads  ./flare-timing/align-time/task-n/p.csv
    Writes ./flare-timing/task-n/discard-further-stop/p.csv

If a list of tasks are supplied then those tasks alone are processed, otherwise
all tasks are processed. The same thing goes if a list of pilots is supplied or
not.
|]
