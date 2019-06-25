module FsRouteOptions (description) where

import Text.RawString.QQ (r)
import Flight.Cmd.Options (Description(..))

description :: Description
description = Description [r|
Extracts the expected or normative optimal route from a competition.
|]
