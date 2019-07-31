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
import qualified Statistics.Sample as Stats (meanVariance)
import qualified Data.Vector as V (fromList, maximum)

import Flight.Earth.Ellipsoid (wgs84)
import Flight.Earth.Sphere (earthRadius)
import Flight.Geodesy (EarthMath(..), EarthModel(..))
import Flight.Comp (Pilot(..))
import Flight.Distance (QTaskDistance, TaskDistance(..))
import Flight.Comp.Distance (GeoNigh(..))
import Flight.Comp.Distance.Double ()
import qualified Flight.Track.Time as Time (TimeRow(..))
import Flight.Track.Distance (TrackDistance(..), TrackReach(..), Nigh)
import Flight.Track.Mask (MaskingReach(..))
import Flight.Score
    ( MinimumDistance(..), PilotDistance(..), FlownMax(..), LinearFraction(..)
    , FlownMax(..), FlownMean(..), FlownStdDev(..), ReachStats(..)
    , linearFraction
    )
import Stats (DashPathInputs(..))
import Flight.Span.Math (Math(..))

maskReachTime
    :: Math
    -> EarthMath
    -> MinimumDistance (Quantity Double [u| km |])
    -> [[(Pilot, TrackReach)]]
    -> [Maybe (QTaskDistance Double [u| m |])]
    -> [Map Pilot (DashPathInputs k)]
    -> [Maybe (QTaskDistance Double [u| m |])]
    -> [[Maybe (Pilot, Time.TimeRow)]]
    -> [[Pilot]]
    -> MaskingReach
maskReachTime Rational _ _ _ _ _ _ _ _ = error "Reach time not yet implemented for rational numbers."
maskReachTime
    Floating
    earthMath
    (MinimumDistance dMin)
    dfNtNigh
    lsWholeTask
    zsTaskTicked
    dsBest
    dsNighRows
    psArriving =
    MaskingReach
        { bolster = zipWith3 ReachStats bsMax bsMean bsStdDev
        , reach = zipWith3 ReachStats rsMax rsMean rsStdDev
        , reachRank = rss
        , nigh = dsNigh
        }
    where
        dsNigh :: [[(Pilot, TrackDistance Nigh)]] =
            compNighTime @Double @Double
                ( earthMath
                , let e = EarthAsEllipsoid wgs84 in case earthMath of
                      Pythagorus -> error "No Pythagorus"
                      Haversines -> EarthAsSphere earthRadius
                      Vincenty -> e
                      AndoyerLambert -> e
                      ForsytheAndoyerLambert -> e
                )
                lsWholeTask
                zsTaskTicked
                dsNighRows

        trackedNigh :: [[(Pilot, TrackReach)]] =
            [
                catMaybes
                $ sequence
                . (fmap (\TrackDistance{made} -> do
                        TaskDistance b <- dBest
                        td@(TaskDistance d) <- made

                        let bd :: FlownMax (Quantity Double [u| km |]) =
                                FlownMax $ convert b
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

        rsNigh :: [[(Pilot, TrackReach)]] = zipWith (++) trackedNigh dfNtNigh

        rsArrive :: [[(Pilot, TrackReach)]] =
            [
                case dBest of
                    Nothing -> []
                    Just td@(TaskDistance b) ->
                        let bd :: FlownMax (Quantity Double [u| km |]) =
                                FlownMax $ convert b

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

        mvs = Stats.meanVariance <$> rssRaw

        rsMean :: [FlownMean (Quantity Double [u| km |])] =
            FlownMean . MkQuantity . fst <$> mvs

        rsStdDev :: [FlownStdDev (Quantity Double [u| km |])] =
            FlownStdDev . MkQuantity . sqrt . snd <$> mvs

        bssRaw =
            [ V.fromList
                [ unQuantity $ Stats.max dMin (convert r :: Quantity _ [u| km |])
                | (_, TrackReach{reach = TaskDistance r}) <- rs
                ]
            | rs <- rss
            ]

        bsMax :: [FlownMax (Quantity Double [u| km |])] =
            [ FlownMax $ if null bs then [u| 0 km |] else MkQuantity $ V.maximum bs
            | bs <- bssRaw
            ]

        bvs = Stats.meanVariance <$> bssRaw

        bsMean :: [FlownMean (Quantity Double [u| km |])] =
            FlownMean . MkQuantity . fst <$> bvs

        bsStdDev :: [FlownStdDev (Quantity Double [u| km |])] =
            FlownStdDev . MkQuantity . sqrt . snd <$> bvs
