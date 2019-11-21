{-# OPTIONS_GHC -fno-warn-orphans #-}

module Flight.Mask.Tag.Double where

import Prelude hiding (span)
import Data.List ((\\))
import Control.Arrow (first)
import Control.Monad (join)

import Flight.Clip (FlyingSection)
import Flight.Units ()
import Flight.Zone.Cylinder (SampleParams(..))
import Flight.Kml (MarkedFixes(..))
import qualified Flight.Kml as Kml (Fix)
import Flight.Track.Cross (ZoneCross(..), ZoneTag(..), TrackFlyingSection(..))
import Flight.Track.Time (ZoneIdx(..))
import Flight.Comp (Task(..))
import Flight.Geodesy.Solution (Trig, GeodesySolutions(..), GeoZones(..))

import Flight.Span.Sliver (GeoSliver(..))
import Flight.Mask.Tag (GeoTag(..))
import Flight.Mask.Internal.Race ()
import Flight.Mask.Internal.Zone
    ( MadeZones(..)
    , SelectedCrossings(..)
    , NomineeCrossings(..)
    , ExcludedCrossings(..)
    , ZoneEntry(..)
    , ZoneExit(..)
    , Crossing
    , TaskZone(..)
    , OrdCrossing(..)
    , slice
    , fixToPoint
    )
import Flight.Mask.Internal.Cross
    ( isStartExit
    , crossingPredicates
    , crossingSelectors
    , tickedZones
    , entersSeq
    , exitsSeq
    , reindex
    )
import Flight.Mask.Interpolate (GeoTagInterpolate(..))
import Flight.Mask.Tag (FnTask, selectZoneCross)
import Flight.Mask.Tag.Prove (TimePass, prove, proveCrossing)
import Flight.Mask.Tag.OrdLists (trimOrdLists)
import Flight.Mask.Tag.Motion (flyingSection, secondsRange, timeRange)

import Flight.ShortestPath.Double ()
import Flight.Span.Double ()
import Flight.Mask.Interpolate.Double ()

instance GeoTagInterpolate Double a => GeoTag Double a where
    started :: Trig Double a => Earth Double -> FnTask k Bool
    started e Task{speedSection, zones} MarkedFixes{fixes} =
        let fromZs = fromZones @Double @Double e
            sepZs = separatedZones @Double @Double e
        in
            case slice speedSection (fromZs zones) of
                [] ->
                    False

                z : _ ->
                    let ez = exitsSeq sepZs z (fixToPoint <$> fixes)
                    in case ez of
                         Right (ZoneExit _ _) : _ -> True
                         _ -> False

    madeGoal :: Trig Double a => Earth Double -> FnTask k Bool
    madeGoal e Task{zones} MarkedFixes{fixes} =
        let zs = fromZones @Double @Double e zones
            sepZs = separatedZones @Double @Double e
        in
            case reverse zs of
                [] ->
                    False

                z : _ ->
                    let ez = entersSeq sepZs z (fixToPoint <$> fixes)
                    in case ez of
                         Left (ZoneEntry _ _) : _ -> True
                         _ -> False

    madeZones
        :: Trig Double a
        => Earth Double
        -> [TimePass]
        -> Task k
        -> MarkedFixes
        -> MadeZones
    madeZones e tps task mf@MarkedFixes{mark0, fixes} =
        MadeZones
            { flying = flying'
            , selectedCrossings = selected
            , nomineeCrossings = nominees
            , excludedCrossings = excluded
            }
        where
            flying' =
                    TrackFlyingSection
                        { loggedFixes = Just len
                        , flyingFixes = flyingIndices
                        , loggedSeconds = snd <$> loggedSeconds
                        , flyingSeconds = flyingSeconds
                        , loggedTimes = loggedTimes
                        , flyingTimes = flyingTimes
                        }

            len = length fixes

            loggedIndices = Just (0, len - 1)
            flyingIndices = flyingSection fixes

            loggedSeconds = secondsRange fixes loggedIndices
            flyingSeconds = secondsRange fixes flyingIndices

            loggedTimes = timeRange mark0 loggedSeconds
            flyingTimes = timeRange mark0 flyingSeconds

            (selected, nominees, excluded) =
                flyingCrossings @Double @Double e tps task mf (flyingFixes flying')

    flyingCrossings
        :: Trig Double a
        => Earth Double
        -> [TimePass]
        -> Task k
        -> MarkedFixes
        -> FlyingSection Int -- ^ The fix indices of the flying section
        -> (SelectedCrossings, NomineeCrossings, ExcludedCrossings)
    flyingCrossings
        e
        timechecks
        task@Task{zones}
        MarkedFixes{mark0, fixes}
        indices =
        (selected, nominees, excluded)
        where
            sepZs = separatedZones @Double @Double e
            fromZs = fromZones @Double @Double e

            keptFixes :: [Kml.Fix]
            keptFixes =
                maybe
                    fixes
                    (\(m, n) -> take (n - m) $ drop m fixes)
                    indices

            xss :: [[Crossing]]
            xss =
                tickedZones
                    fs
                    (fromZs zones)
                    (fixToPoint <$> keptFixes)

            xss' :: [[Crossing]]
            xss' =
                maybe
                    xss
                    (\(ii, _) -> (fmap . fmap) (reindex ii) xss)
                    (first ZoneIdx <$> indices)

            css = f (const True) <$> xss'
            nss =
                [ f timecheck xs
                | timecheck <- timechecks
                | xs <- xss'
                ]

            nominees = NomineeCrossings nss
            excluded = ExcludedCrossings $ css \\ nss

            yss :: [[OrdCrossing]]
            yss = trimOrdLists ((fmap . fmap) OrdCrossing xss')

            yss' :: [[Crossing]]
            yss' = (fmap . fmap) unOrdCrossing yss

            selected =
                SelectedCrossings
                [
                    let prover = proveCrossing timecheck mark0 fixes
                    in selectZoneCross prover selector ys

                | timecheck <- timechecks
                | selector <- selectors
                | ys <- yss'
                ]

            fs =
                (\x ->
                    let b = isStartExit sepZs fromZs x
                    in crossingPredicates sepZs b x) task

            f :: TimePass -> [Crossing] -> [Maybe ZoneCross]
            f _ [] = []

            f tp (Right (ZoneExit m n) : es) =
                p : f tp es
                where
                    p = prove tp mark0 fixes m n [True, False]

            f tp (Left (ZoneEntry m n) : es) =
                p : f tp es
                where
                    p = prove tp mark0 fixes m n [False, True]

            selectors :: [[Crossing] -> Maybe Crossing]
            selectors =
                (\x ->
                    let b = isStartExit sepZs fromZs x
                    in crossingSelectors b x) task

    tagZones
        :: Trig Double a
        => Earth Double
        -> SampleParams Double
        -> [TaskZone Double]
        -> [Maybe ZoneCross]
        -> [Maybe ZoneTag]
    tagZones e sp zs cs =
        [ join $ g z <$> c
        | z <- zs
        | c <- cs
        ]
        where
            tag = crossingTag @Double @Double e sp

            g z c@ZoneCross{crossingPair, inZone} =
                case (crossingPair, inZone) of
                    ([x, y], [a, b]) -> do
                        i <- tag z (x, y) (a, b)
                        return $ ZoneTag{inter = i, cross = c}

                    _ -> Nothing
