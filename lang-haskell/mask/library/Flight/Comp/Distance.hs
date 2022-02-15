{-|
Module      : Flight.Comp.Distance
Copyright   : (c) Block Scope Limited 2018
License     : MPL-2.0
Maintainer  : phil.dejoux@blockscope.com
Stability   : experimental

Distances over all pilots.
-}
module Flight.Comp.Distance
    ( GeoNigh(..)
    , DashPathInputs(..)
    , compDistance
    , compNighTick
    ) where

import Prelude hiding (span)
import Data.Maybe (mapMaybe, catMaybes, isJust)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (fromList, lookup)
import Data.UnitsOfMeasure ((-:), u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Distance (QTaskDistance, TaskDistance(..), toKm, unTaskDistanceAsKm)
import Flight.Comp (Pilot, MadeGoal(..), LandedOut(..))
import Flight.Zone.Cylinder (SampleParams(..))
import Flight.Zone.Raw (Give)
import Flight.Route (TrackLine(..))
import "flight-gap-allot" Flight.Score (BestTime(..), MinimumDistance(..))
import Flight.Track.Time (LeadTick(..))
import Flight.Track.Distance (TrackDistance(..), Effort, Nigh)
import qualified Flight.Track.Time as Time (TimeRow(..), TickRow(..))
import Flight.Geodesy.Solution (Trig, GeodesySolutions(..))

import Flight.Mask.Distance (GeoDash(..))
import Flight.Comp.Distance.Line (DashPathInputs(..), tickNighTrackLine)

class GeoDash g a => GeoNigh g a where
    -- | How near did a pilot get to goal during the flight. This can be closer
    -- to goal than the landing spot if the pilot flew away from goal to land.
    compNighTime
        :: Trig g a
        => Earth g
        -> Maybe Give
        -> SampleParams g
        -> [Maybe (QTaskDistance g [u| m |])]
        -> [Map Pilot (DashPathInputs k)]
        -> [[Maybe (Pilot, Time.TimeRow)]]
        -> [[(Pilot, TrackDistance Nigh)]]

    timeNighTrackLine
        :: Trig g a
        => Earth g
        -> Maybe Give
        -> SampleParams g
        -> Maybe (QTaskDistance Double [u| m |])
        -> Map Pilot (DashPathInputs k)
        -> (Pilot, Time.TimeRow)
        -> (Pilot, TrackDistance Nigh)

    pathToGo
        :: Trig g a
        => Earth g
        -> Maybe Give
        -> SampleParams g
        -> DashPathInputs k
        -> Time.TimeRow
        -> Quantity g [u| m |]
        -> TrackLine

compDistance
    -- | The minimum distance for the comp, the same for each task.
    :: MinimumDistance (Quantity Double [u| km |])
    -- | The distance of each task.
    -> [Maybe (QTaskDistance Double [u| m |])]
    -- | The pilots arriving at goal for each task.
    -> [MadeGoal]
    -- | The pilots landing out for each task.
    -> [LandedOut]
    -- | The best time to make goal for each task.
    -> [Maybe (BestTime (Quantity Double [u| h |]))]
    -- | For each task, for each pilot in the landout group, the
    -- row closest to goal.
    -> [[Maybe (Pilot, Time.TickRow)]]
    ->
        ( [Maybe (QTaskDistance Double [u| m |])]
        -- ^ The sum of distance over min of those arriving at goal.
        , [Maybe (QTaskDistance Double [u| m |])]
        -- ^ The sum of distance over min of those landing out.
        , [Maybe (QTaskDistance Double [u| m |])]
        -- ^ The best distance
        , [[Maybe (Pilot, Maybe LeadTick)]]
        -- ^ The lead time of the point closest to goal.
        )
compDistance dMin lsTask psArriving psLandingOut tsBest rows =
    ( (fmap . fmap) (TaskDistance . MkQuantity . (1000000 *)) dsSumArriving
    , (fmap . fmap) (TaskDistance . MkQuantity . (1000000 *)) dsSumLandingOut
    , (fmap . fmap) (TaskDistance . MkQuantity . (1000 *)) dsBest
    , rowTicks
    )
    where
        MinimumDistance (MkQuantity dMin') = dMin

        -- Distances (ds) of point in the flight closest to goal.
        dsNigh :: [[(Pilot, TrackDistance Effort)]] =
                zipWith3
                    lookupTaskBestDistance
                    (Map.fromList . catMaybes <$> rows)
                    lsTask
                    (unLandedOut <$> psLandingOut)

        -- For each task, the best distance made.
        dsMade :: [Maybe Double] =
                (\xs -> if null xs then Nothing else Just . maximum $ xs)
                . catMaybes
                <$> (fmap . fmap) (fmap toKm . made . snd) dsNigh

        -- If even a single pilot makes goal then the best distance is the task
        -- distance.
        dsBest :: [Maybe Double] =
                zipWith3
                    (\l t d -> if isJust t then unTaskDistanceAsKm <$> l else d)
                    lsTask
                    tsBest
                    dsMade

        -- The sum of distance of those pilots landing out but this does not
        -- include the sum of distance of those pilots making goal.
        dsSumLandingOut :: [Maybe Double] =
                [ \case [] -> Nothing; xs -> Just . sum $ xs
                  $ (\d -> max 0 (d - dMin'))
                  <$> ds
                | ds <- catMaybes <$> (fmap . fmap) (fmap toKm . made . snd) dsNigh
                ]

        dsTaskOverMin =
                [ do
                    l' <- l
                    return . max 0 $ l' - dMin'
                | l <- (fmap . fmap) unTaskDistanceAsKm lsTask
                ]

        dsSumArriving:: [Maybe Double] =
                [ if null ps then Nothing else do
                    dt' <- dt
                    return $ fromIntegral (length ps) * dt'
                | dt <- dsTaskOverMin
                | ps <- unMadeGoal <$> psArriving
                ]

        rowTicks :: [[Maybe (Pilot, Maybe LeadTick)]] =
                (fmap . fmap . fmap)
                    (fmap (\Time.TickRow{tickLead} -> tickLead))
                    rows

compNighTick
    :: [Maybe (QTaskDistance Double [u| m |])]
    -> [Map Pilot (DashPathInputs k)]
    -> [[Maybe (Pilot, Time.TickRow)]]
    -> [[(Pilot, TrackDistance Nigh)]]
compNighTick lsTask zsTaskTicked rows =
        [ tickNighTrackLine td zs <$> xs
        | td <- lsTask
        | zs <- zsTaskTicked
        | xs <- (catMaybes <$> rows)
        ]

lookupTaskBestDistance
    :: Map Pilot Time.TickRow
    -> Maybe (QTaskDistance Double [u| m |])
    -> [Pilot]
    -> [(Pilot, TrackDistance Effort)]
lookupTaskBestDistance m td =
    sortOn (togo . snd)
    . mapMaybe (lookupPilotBestDistance m td)

lookupPilotBestDistance
    :: Map Pilot Time.TickRow
    -> Maybe (QTaskDistance Double [u| m |])
    -> Pilot
    -> Maybe (Pilot, TrackDistance Effort)
lookupPilotBestDistance m td p =
    (p,) . madeDistance td <$> Map.lookup p m

madeDistance
    :: Maybe (QTaskDistance Double [u| m |])
    -> Time.TickRow
    -> TrackDistance Effort

madeDistance Nothing Time.TickRow{togo = d} =
    TrackDistance
        { togo = Just . TaskDistance . togoFromKms $ d
        , made = Nothing
        }

madeDistance (Just (TaskDistance td)) Time.TickRow{togo = d} =
    TrackDistance
        { togo = Just . TaskDistance $ togo'
        , made = Just . TaskDistance $ td -: togo'
        }
    where
        togo' = togoFromKms d

togoFromKms :: Double -> Quantity Double [u| m |]
togoFromKms d =
    convert d' :: Quantity Double [u| m |]
    where
        d' :: Quantity Double [u| km |]
        d' = MkQuantity d
