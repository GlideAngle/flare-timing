{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

module Mask.Reach.Time (maskReachTime) where

import Data.Maybe (catMaybes)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.UnitsOfMeasure (u, convert, unQuantity)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Statistics.Sample (mean, stdDev)
import qualified Data.Vector as V (fromList)

import Flight.Comp (Pilot(..))
import Flight.Distance (QTaskDistance, TaskDistance(..))
import Flight.Comp.Distance (compNighTime)
import qualified Flight.Track.Time as Time (TimeRow(..))
import Flight.Track.Distance (TrackDistance(..), TrackReach(..), Nigh)
import Flight.Track.Mask (MaskingReach(..))
import Flight.Score
    ( MinimumDistance(..), PilotDistance(..), BestDistance(..), LinearFraction(..)
    , FlownMax(..)
    , linearFraction
    )
import Stats (DashPathInputs(..))

maskReachTime
    :: MinimumDistance (Quantity Double [u| km |])
    -> [Maybe (QTaskDistance Double [u| m |])]
    -> [Map Pilot (DashPathInputs k)]
    -> [Maybe (QTaskDistance Double [u| m |])]
    -> [[Maybe (Pilot, Time.TimeRow)]]
    -> [[Pilot]]
    -> MaskingReach
maskReachTime (MinimumDistance dMin) lsWholeTask zsTaskTicked dsBest dsNighRows psArriving =
    MaskingReach
        { flownMax = dsFlownMax
        , flownMean = fsMean
        , flownStdDev = fsStdDev
        , reachMean = rsMean
        , reachStdDev = rsStdDev
        , reachRank = rss
        , nigh = dsNigh
        }
    where
        dsFlownMax = (fmap . fmap) (\(TaskDistance d) -> FlownMax d) dsBest

        dsNigh :: [[(Pilot, TrackDistance Nigh)]] =
            compNighTime lsWholeTask zsTaskTicked dsNighRows

        rsNigh :: [[(Pilot, TrackReach)]] =
            [
                catMaybes
                $
                    (\case
                        (_, Nothing) -> Nothing
                        (a, Just b) -> Just (a, b))
                    . (fmap (\TrackDistance{made} -> do
                            TaskDistance b <- dBest
                            td@(TaskDistance d) <- made

                            let bd :: BestDistance (Quantity Double [u| km |]) =
                                    BestDistance $ convert b
                            let pd :: PilotDistance (Quantity Double [u| km |]) =
                                    PilotDistance $ convert d

                            return
                                TrackReach
                                    { reach = td
                                    , frac = linearFraction bd pd
                                    }))
                    <$> ds

            | dBest <- dsBest
            | ds <- dsNigh
            ]

        rsArrive :: [[(Pilot, TrackReach)]] =
            [
                case dBest of
                    Nothing -> []
                    Just td@(TaskDistance b) ->
                        let bd :: BestDistance (Quantity Double [u| km |]) =
                                BestDistance $ convert b

                            pd :: PilotDistance (Quantity Double [u| km |]) =
                                PilotDistance $ convert b

                            tr =
                                TrackReach
                                    { reach = td
                                    , frac = linearFraction bd pd
                                    }
                        in (flip (,)) tr <$> ps

            | dBest <- dsBest
            | ps <- psArriving
            ]

        rss :: [[(Pilot, TrackReach)]] =
            [
                sortOn
                    ( negate
                    . (\TrackReach{frac = LinearFraction lf} -> lf)
                    . snd
                    )
                $ xs ++ ys
            | xs <- rsArrive
            | ys <- rsNigh
            ]

        rssRaw =
            [ V.fromList
                [ unQuantity r | (_, TrackReach{reach = TaskDistance r}) <- rs]
            | rs <- rss
            ]

        rsMean :: [QTaskDistance Double [u| m |]] = TaskDistance . MkQuantity . mean <$> rssRaw
        rsStdDev  :: [QTaskDistance Double [u| m |]] = TaskDistance . MkQuantity . stdDev <$> rssRaw

        fssRaw =
            [ V.fromList
                [ unQuantity $ max r (convert dMin) | (_, TrackReach{reach = TaskDistance r}) <- rs]
            | rs <- rss
            ]

        fsMean :: [QTaskDistance Double [u| m |]] = TaskDistance . MkQuantity . mean <$> fssRaw
        fsStdDev  :: [QTaskDistance Double [u| m |]] = TaskDistance . MkQuantity . stdDev <$> fssRaw
