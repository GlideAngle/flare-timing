module Serve.Validity (nullValidityWorking) where

import Data.UnitsOfMeasure (u)
import "flight-gap-allot" Flight.Score
import "flight-gap-valid" Flight.Score

nullLaunchValidityWorking :: LaunchValidityWorking
nullLaunchValidityWorking =
    LaunchValidityWorking
        { flying = PilotsFlying 0
        , present = PilotsPresent 0
        , nominalLaunch = NominalLaunch 0
        }

nullDistanceValidityWorking :: DistanceValidityWorking
nullDistanceValidityWorking =
    DistanceValidityWorking
        { sum = SumOfDistance [u| 0 km |]
        , flying = PilotsFlying 0
        , area = NominalDistanceArea 0
        , nominalGoal = NominalGoal 0
        , nominalDistance = NominalDistance [u| 0 km |]
        , minimumDistance = MinimumDistance [u| 0 km |]
        , reachMax =
            ReachToggle
                { extra = FlownMax [u| 0 km |]
                , flown = FlownMax [u| 0 km |]
                }
        }

nullTimeValidityWorking :: TimeValidityWorking
nullTimeValidityWorking =
    TimeValidityWorking
        { ssBestTime = Nothing
        , gsBestTime = Nothing
        , nominalTime = NominalTime [u| 0 h |]
        , nominalDistance = NominalDistance [u| 0 km |]
        , reachMax =
            ReachToggle
                { extra = FlownMax [u| 0 km |]
                , flown = FlownMax [u| 0 km |]
                }
        }

nullValidityWorking :: ValidityWorking
nullValidityWorking =
    ValidityWorking
        { launch = nullLaunchValidityWorking
        , distance = nullDistanceValidityWorking
        , time = nullTimeValidityWorking
        , stop = Nothing
        }
