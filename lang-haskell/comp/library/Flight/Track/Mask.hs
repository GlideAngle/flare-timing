{-|
Module      : Flight.Track.Mask
Copyright   : (c) Block Scope Limited 2017
License     : MPL-2.0
Maintainer  : phil.dejoux@blockscope.com
Stability   : experimental

Tracks masked with task control zones.
-}
module Flight.Track.Mask
    ( RaceTime(..)
    , module Flight.Track.Mask.Arrival
    , module Flight.Track.Mask.Effort
    , module Flight.Track.Mask.Reach
    , module Flight.Track.Mask.Speed
    , module Flight.Track.Mask.Lead
    ) where

import Flight.Track.Mask.Arrival
import Flight.Track.Mask.Effort
import Flight.Track.Mask.Lead
import Flight.Track.Mask.Reach
import Flight.Track.Mask.Speed
