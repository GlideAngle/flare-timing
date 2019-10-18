module FsEffortOptions (description) where

import Text.RawString.QQ (r)
import Flight.Cmd.Options (Description(..))

description :: Description
description = Description [r|
Extracts the distance difficulty chunking from a competition.
|]
