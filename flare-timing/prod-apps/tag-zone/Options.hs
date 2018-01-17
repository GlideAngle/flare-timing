{-# LANGUAGE QuasiQuotes #-}

module Options (description) where

import Text.RawString.QQ (r)
import Flight.Cmd.Options (Description(..))

description :: Description
description = Description [r|
For each crossing, given as a pair of fixes, interpolates the time and place
where it touches the control zone.

Where 'c' is the comp name and '.' is the folder with competition inputs;
    Reads  ./c.cross-zone.yaml
    Writes ./c.tag-zone.yaml

If a list of tasks are supplied then those tasks alone are processed, otherwise
all tasks are processed. The same thing goes if a list of pilots is supplied or
not.
|]
