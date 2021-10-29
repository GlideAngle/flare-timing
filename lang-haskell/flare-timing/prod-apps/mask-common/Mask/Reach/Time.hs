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
import qualified Data.Vector as V (fromList, maximum)
import Mask.Reach.Tick (maybeMeanVariance, maybeReachStats)

import Flight.Zone.Cylinder (SampleParams(..), Samples(..), Tolerance(..))
import Flight.Zone.Raw (Give)
import Flight.Earth.Ellipsoid (wgs84)
import Flight.Earth.Sphere (earthRadius)
import Flight.Geodesy (EarthMath(..), EarthModel(..), Projection(..))
import Flight.Comp (Pilot(..))
import Flight.Distance (QTaskDistance, TaskDistance(..))
import Flight.Comp.Distance (GeoNigh(..))
import Flight.Comp.Distance.Double ()
import qualified Flight.Track.Time as Time (TimeRow(..))
import Flight.Track.Distance (TrackDistance(..), TrackReach(..), Nigh)
import Flight.Track.Mask (CompMaskingReach(..))
import "flight-gap-allot" Flight.Score
    ( MinimumDistance(..), PilotDistance(..), FlownMax(..), LinearFraction(..)
    , FlownMax(..), FlownMean(..), FlownStdDev(..)
    , linearFraction
    )
import Stats (DashPathInputs(..))
import Flight.Span.Math (Math(..))

sp :: SampleParams Double
sp = SampleParams (replicate 6 $ Samples 11) (Tolerance 0.03)

maskReachTime
    :: Math
    -> EarthMath
    -> Maybe Give
    -> MinimumDistance (Quantity Double [u| km |])
    -> [[(Pilot, TrackReach)]]
    -> [Maybe (QTaskDistance Double [u| m |])]
    -> [Map Pilot (DashPathInputs k)]
    -> [Maybe (QTaskDistance Double [u| m |])]
    -> [[Maybe (Pilot, Time.TimeRow)]]
    -> [[Pilot]]
    -> CompMaskingReach
maskReachTime Rational _ _ _ _ _ _ _ _ _ = error "Reach time not yet implemented for rational numbers."
maskReachTime
    Floating
    earthMath
    give
    (MinimumDistance dMin)
    dfNtNigh
    lsWholeTask
    zsTaskTicked
    dsBest
    dsNighRows
    psArriving =
    CompMaskingReach
        { bolster = zipWith3 maybeReachStats bsMax bsMean bsStdDev
        , reach = zipWith3 maybeReachStats rsMax rsMean rsStdDev
        , reachRank = rss
        , nigh = dsNigh
        }
    where
        dsNigh :: [[(Pilot, TrackDistance Nigh)]] =
            compNighTime @Double @Double
                ( earthMath
                , let e = EarthAsEllipsoid wgs84 in case earthMath of
                      Pythagorus -> EarthAsFlat UTM
                      Haversines -> EarthAsSphere earthRadius
                      Vincenty -> e
                      AndoyerLambert -> e
                      ForsytheAndoyerLambert -> e
                      FsAndoyer -> e
                )
                give
                sp
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

        mvs = maybeMeanVariance <$> rssRaw

        rsMean :: [Maybe (FlownMean (Quantity Double [u| km |]))] =
            fmap (FlownMean . MkQuantity . fst) <$> mvs

        rsStdDev :: [Maybe (FlownStdDev (Quantity Double [u| km |]))] =
            fmap (FlownStdDev . MkQuantity . sqrt . snd) <$> mvs

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

        bvs = maybeMeanVariance <$> bssRaw

        bsMean :: [Maybe (FlownMean (Quantity Double [u| km |]))] =
            fmap (FlownMean . MkQuantity . fst) <$> bvs

        bsStdDev :: [Maybe (FlownStdDev (Quantity Double [u| km |]))] =
            fmap (FlownStdDev . MkQuantity . sqrt . snd) <$> bvs
