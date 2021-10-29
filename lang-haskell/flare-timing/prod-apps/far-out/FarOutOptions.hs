module FarOutOptions (description) where

import Text.RawString.QQ (r)
import Flight.Cmd.Options (Description(..))

description :: Description
description = Description [r|
Group and count far outs.

Where 'c' is the comp name and '.' is the folder with competition inputs;
    Reads  ./c.mask-track.yaml
    Writes ./c.far-out.yaml

If a list of tasks are supplied then those tasks alone are processed, otherwise
all tasks are processed.

The same thing goes if a list of pilots is supplied or not.
|]
