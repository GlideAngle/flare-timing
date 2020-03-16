module MaskLead (raceTimes) where

import Flight.Clip (FlyCut(..), FlyClipping(..))
import Flight.Comp
    ( IxTask(..)
    , Task(..)
    , TaskStop(..)
    )
import qualified Flight.Lookup as Lookup (compRaceTimes)
import Flight.Lookup.Tag (TaskLeadingLookup(..))
import Flight.Track.Mask (RaceTime(..))

raceTimes :: TaskLeadingLookup -> [IxTask] -> [Task k] -> [Maybe RaceTime]
raceTimes lookupTaskLeading iTasks tasks =
    [ do
        rt@RaceTime{..} <- crt
        return $
            maybe
                rt
                (\stp ->
                    uncut . clipToCut $
                        FlyCut
                            { cut = Just (openTask, min stp closeTask)
                            , uncut = rt
                            })
                (retroactive <$> stopped task)

    | crt <- Lookup.compRaceTimes lookupTaskLeading iTasks tasks
    | task <- tasks
    ]
