{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.Fsdb.Distance
    ( taskKmToMetres
    , taskMetresToKm
    , asAwardReach
    ) where

import Data.UnitsOfMeasure ((/:), u, convert, unQuantity)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Flight.Distance (TaskDistance(..), QTaskDistance)
import Flight.Track.Distance (AwardedDistance(..))

taskKmToMetres
    :: QTaskDistance Double [u| km |]
    -> QTaskDistance Double [u| m |]
taskKmToMetres (TaskDistance d) = TaskDistance . convert $ d

taskMetresToKm
    :: QTaskDistance Double [u| m |]
    -> QTaskDistance Double [u| km |]
taskMetresToKm (TaskDistance d) = TaskDistance . convert $ d

asAwardReach
    :: String
    -> Maybe (QTaskDistance Double [u| km |])
    -> Maybe AwardedDistance
asAwardReach t' m' = do
    -- TODO: Use unpickling for FsTaskScoreParams/@task_distance.
    -- WARNING: Having some trouble with unpickling task distance.
    -- Going with simple read for now.
    let td :: Double = read t'
    let t@(TaskDistance qt) = taskKmToMetres . TaskDistance . MkQuantity $ td
    m@(TaskDistance qr) <- taskKmToMetres <$> m'

    return $ AwardedDistance
        { awardedMade = m
        , awardedTask = t
        , awardedFrac = unQuantity $ qr /: qt
        }
