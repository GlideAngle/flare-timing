{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Mask.Reach.Time (maskReachTime) where

import Prelude hiding (max)
import qualified Prelude as Stats (max)
import Data.Maybe (catMaybes)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.UnitsOfMeasure (u, convert, unQuantity)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import qualified Statistics.Sample as Stats (mean, stdDev)
import qualified Data.Vector as V (fromList, maximum)

import Flight.Comp (Pilot(..))
import Flight.Distance (QTaskDistance, TaskDistance(..))
import Flight.Comp.Distance (compNighTime)
import qualified Flight.Track.Time as Time (TimeRow(..))
import Flight.Track.Distance (TrackDistance(..), TrackReach(..), Nigh)
import Flight.Track.Mask (MaskingReach(..))
import Flight.Score
    ( MinimumDistance(..), PilotDistance(..), BestDistance(..), LinearFraction(..)
    , FlownMax(..), FlownMean(..), FlownStdDev(..), ReachStats(..)
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
        { bolster = zipWith3 ReachStats bsMax bsMean bsStdDev
        , reach = zipWith3 ReachStats rsMax rsMean rsStdDev
        , reachRank = rss
        , nigh = dsNigh
        }
    where
        dsNigh :: [[(Pilot, TrackDistance Nigh)]] =
            compNighTime lsWholeTask zsTaskTicked dsNighRows

        rsNigh :: [[(Pilot, TrackReach)]] =
            [
                catMaybes
                $ sequence
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
                [ unQuantity (convert r :: Quantity _ [u| km |])
                | (_, TrackReach{reach = TaskDistance r}) <- rs
                ]
            | rs <- rss
            ]

        rsMax :: [FlownMax (Quantity Double [u| km |])] =
            [ FlownMax $ if null rs then [u| 0 km |] else MkQuantity $ V.maximum rs
            | rs <- rssRaw
            ]

        rsMean :: [FlownMean (Quantity Double [u| km |])] =
            FlownMean . MkQuantity . Stats.mean <$> rssRaw

        rsStdDev  :: [FlownStdDev (Quantity Double [u| km |])] =
            FlownStdDev . MkQuantity . Stats.stdDev <$> rssRaw

        fssRaw =
            [ V.fromList
                [ unQuantity $ Stats.max dMin (convert r :: Quantity _ [u| km |])
                | (_, TrackReach{reach = TaskDistance r}) <- rs
                ]
            | rs <- rss
            ]

        bsMax :: [FlownMax (Quantity Double [u| km |])] =
            [ FlownMax $ if null fs then [u| 0 km |] else MkQuantity $ V.maximum fs
            | fs <- fssRaw
            ]

        bsMean :: [FlownMean (Quantity Double [u| km |])] =
            FlownMean . MkQuantity . Stats.mean <$> fssRaw

        bsStdDev  :: [FlownStdDev (Quantity Double [u| km |])] =
            FlownStdDev . MkQuantity . Stats.stdDev <$> fssRaw
