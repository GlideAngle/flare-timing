module FsTrimOptions (description) where

import Text.RawString.QQ (r)
import Flight.Cmd.Options (Description(..))

description :: Description
description = Description [r|
Filters a competition *.fsdb file keeping only the XML nodes needed for scoring
comparison.
|]
