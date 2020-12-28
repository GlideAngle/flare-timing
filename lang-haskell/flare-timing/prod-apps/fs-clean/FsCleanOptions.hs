module FsCleanOptions (description) where

import Text.RawString.QQ (r)
import Flight.Cmd.Options (Description(..))

description :: Description
description = Description [r|
Cleans a competition *.fsdb file of participant (pilot) information, keeping
only their name.
|]
