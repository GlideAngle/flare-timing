module FsArrivalOptions (description) where

import Text.RawString.QQ (r)
import Flight.Cmd.Options (Description(..))

description :: Description
description = Description [r|
Extracts the arrival time, position and fraction from a competition.
|]
