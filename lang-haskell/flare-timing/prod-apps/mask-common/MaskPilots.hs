{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
module MaskPilots (maskPilots, didFlyNoTrackStats) where

import Data.Function (on)
import Data.Maybe (isJust)
import Data.List (sortOn, groupBy, partition)
import Data.UnitsOfMeasure ((-:), u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import qualified Flight.Comp as Cmp (DfNoTrackPilot(..))
import Flight.Comp
    ( Pilot(..)
    , PilotGroup(didFlyNoTracklog)
    , Task(..)
    , TaskRouteDistance(..)
    , DfNoTrack(..)
    , StartGate(..)
    , StartEnd(..)
    )
import Flight.Distance (QTaskDistance, TaskDistance(..))
import Flight.Track.Place (reIndex)
import Flight.Track.Time (AwardedVelocity(..))
import Flight.Track.Distance
    (TrackDistance(..), AwardedDistance(..), Clamp(..), Effort)
import qualified Flight.Track.Distance as Track (awardByFrac)
import Flight.Track.Speed (pilotTime)
import "flight-gap-allot" Flight.Score (ArrivalPlacing(..), MinimumDistance(..))
import "flight-gap-valid" Flight.Score (ReachToggle(..))
import Stats (TimeStats(..), FlightStats(..), nullStats)

awardByFrac
    :: Clamp
    -> QTaskDistance Double [u| m |]
    -> AwardedDistance
    -> Quantity Double [u| m |]
awardByFrac c td a = convert $ Track.awardByFrac c td a

madeAwarded :: QTaskDistance Double [u| m |] -> Effort -> TrackDistance Effort
madeAwarded (TaskDistance td) d@(TaskDistance d') =
    TrackDistance
        { togo = Just . TaskDistance $ td -: d'
        , made = Just d
        }

rankByArrival
    :: [(Pilot, FlightStats _)]
    -> [(Pilot, FlightStats _)]
    -> [(Pilot, FlightStats _)]
rankByArrival xsDf xsDfNt =
    case any isJust yTs of
        False -> xsDf ++ xsDfNt
        True ->
            [ (rankArrival f ii ) <$> y
            | (ii, ys) <-
                        reIndex
                        . zip [1..]
                        . groupBy ((==) `on` (fmap Stats.esMark) . statTimeRank . snd)
                        $ xs
            , let f =
                    if length ys == 1
                        then ArrivalPlacing
                        else (\x -> ArrivalPlacingEqual x (fromIntegral $ length ys))
            , y <- ys
            ]
            ++ xsLandout
    where
        yTs = statTimeRank . snd <$> xsDfNt

        xs :: [(Pilot, FlightStats _)]
        xs =
            sortOn ((fmap Stats.esMark) . statTimeRank . snd)
            $ xsArrived

        (xsArrived, xsLandout) =
            partition (\(_, FlightStats{statTimeRank = r}) -> isJust r)
            $ xsDf ++ xsDfNt

rankArrival :: (Integer -> ArrivalPlacing) -> Integer -> FlightStats _ -> FlightStats _
rankArrival _ _ x@FlightStats{statTimeRank = Nothing} = x
rankArrival f ii x@FlightStats{statTimeRank = Just y} =
    x{statTimeRank = Just y{positionAtEss = Just $ f ii}}

maskPilots
    :: MinimumDistance (Quantity Double [u| km |])
    -> [Task k]
    -> [Maybe TaskRouteDistance]
    -> [PilotGroup]
    -> [[Either (Pilot, b) (Pilot, Pilot -> FlightStats w)]]
    -> [[(Pilot, FlightStats w)]]
maskPilots free tasks lsTask pilotGroups fss =
    [ rankByArrival ysDf ysDfNt
    | ysDf <- yssDf
    | ysDfNt <- didFlyNoTrackStats free tasks lsTask dfNtss
    ]
    where
        dfNtss = didFlyNoTracklog <$> pilotGroups

        fssDf =
            [ let ps = Cmp.pilot <$> dfNts in
              filter
                  ( not
                  . (`elem` ps)
                  . (\case Left (p, _) -> p; Right (p, _) -> p))
                  flights
            | flights <- fss
            | DfNoTrack dfNts <- dfNtss
            ]

        yssDf :: [[(Pilot, FlightStats _)]] =
            [ fmap
                (\case
                    Left (p, _) -> (p, nullStats)
                    Right (p, g) -> (p, g p))
                flights
            | flights <- fssDf
            ]

didFlyNoTrackStats
    :: MinimumDistance (Quantity Double [u| km |])
    -> [Task k]
    -> [Maybe TaskRouteDistance]
    -> [DfNoTrack]
    -> [[(Pilot, FlightStats w)]]
didFlyNoTrackStats (MinimumDistance dMin) tasks lsTask dfNtss =
    [
        fmap
            (\Cmp.DfNoTrackPilot
                { pilot = p
                , awardedReach = dA
                , awardedVelocity = AwardedVelocity{ss, es}
                } ->
                let dm :: Quantity Double [u| m |] = convert dMin

                    d = TaskDistance
                        <$> maybe
                            (Just dm)
                            (\ReachToggle{extra = dAward} -> do
                                td <- lTask
                                let a = awardByFrac (Clamp True) td dAward

                                return $ max a dm)
                            dA

                    sEffort = madeAwarded <$> lTask <*> d

                    sTime =
                        case (ss, es) of
                            (Just ss', Just es') ->
                                let se = StartEnd ss' es
                                    ssT = pilotTime [StartGate ss'] se
                                    gsT = pilotTime gates se
                                in
                                    do
                                        ssT' <- ssT
                                        gsT' <- gsT
                                        return
                                            TimeStats
                                                { ssTime = ssT'
                                                , gsTime = gsT'
                                                , esMark = es'
                                                , positionAtEss = Nothing
                                                }
                            _ -> Nothing

                in (p, nullStats{statEffort = sEffort, statTimeRank = sTime}))
            dfNts
    | DfNoTrack dfNts <- dfNtss
    | lTask <- (fmap. fmap) wholeTaskDistance lsTask
    | gates <- startGates <$> tasks
    ]
