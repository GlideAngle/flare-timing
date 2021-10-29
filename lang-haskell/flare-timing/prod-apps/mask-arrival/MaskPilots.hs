﻿{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
module MaskPilots (maskPilots) where

import Data.Function (on)
import Data.Maybe (isJust)
import Data.List (sortOn, groupBy, partition)
import Data.UnitsOfMeasure (u)
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
import Flight.Track.Place (reIndex)
import Flight.Track.Time (AwardedVelocity(..))
import Flight.Track.Speed (pilotTime)
import "flight-gap-allot" Flight.Score (ArrivalPlacing(..), MinimumDistance(..))
import Stats (TimeStats(..), FlightStats(..), nullStats)

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
maskPilots _dMin tasks _lsTask pilotGroups fss =
    [ rankByArrival ysDf ysDfNt
    | ysDf <- yssDf
    | ysDfNt <- yssDfNt
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

        yssDfNt :: [[(Pilot, FlightStats _)]] =
            [
                fmap
                    (\Cmp.DfNoTrackPilot
                        { pilot = p
                        , awardedVelocity = AwardedVelocity{ss, es}
                        } ->
                        let sTime =
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

                        in (p, nullStats{statTimeRank = sTime}))
                    dfNts
            | DfNoTrack dfNts <- dfNtss
            | gates <- startGates <$> tasks
            ]
