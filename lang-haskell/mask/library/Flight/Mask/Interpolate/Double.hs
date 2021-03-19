{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Flight.Mask.Interpolate.Double () where

import Prelude hiding (span)
import Data.Time.Clock (NominalDiffTime, addUTCTime, diffUTCTime)
import Data.UnitsOfMeasure (u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Units ()
import Flight.LatLng (Lat(..), Lng(..), LatLng(..))
import Flight.LatLng.Raw (RawLat(..), RawLng(..), RawAlt(..))
import Flight.Zone (Zone(..))
import Flight.Zone.Path (distancePointToPoint)
import Flight.Zone.Cylinder (SampleParams(..))
import qualified Flight.Track.Cross as Cg (Fix(..))
import Flight.Track.Cross (InterpolatedFix(..))
import Flight.Distance (TaskDistance(..), PathDistance(..))
import Flight.Task (Zs(..))
import Flight.Geodesy.Solution (Trig, GeodesySolutions(..))
import Flight.ShortestPath (GeoPath(..))

import Flight.Span.Sliver (GeoSliver(..), Sliver(..))
import Flight.Mask.Internal.Zone (TaskZone(..), fixToRadLL)
import Flight.Mask.Interpolate (GeoTagInterpolate(..), linearInterpolate)

import Flight.ShortestPath.Double ()
import Flight.Span.Double ()

instance GeoSliver Double a => GeoTagInterpolate Double a where

    interpolate
        :: Trig Double a
        => Earth Double
        -> SampleParams Double
        -> TaskZone Double
        -> LatLng Double [u| rad |]
        -> LatLng Double [u| rad |]
        -> Zs ([LatLng Double [u| rad |]])
    interpolate e sp (TaskZone z) x y =
        vertices . snd <$> ee
        where
            Sliver{..} = sliver @Double @Double e
            zs' = [Point x, z, Point y]
            ac = angleCut @Double @Double e
            dEE = shortestPath @Double @Double e
            ee = dEE cseg cs ac sp Nothing zs'

    fractionate
        :: Trig Double a
        => Earth Double
        -> Zs ([LatLng Double [u| rad |]])
        -> Maybe (LatLng Double [u| rad |], Double)
    fractionate e (Zs [x, xy, y]) =
        Just (xy, realToFrac $ d0 / d1)
        where
            Sliver{..} = sliver @Double @Double e
            f xs = edgesSum $ distancePointToPoint span xs
            TaskDistance (MkQuantity d0) = f [Point x, Point xy]
            TaskDistance (MkQuantity d1) = f [Point x, Point y]

    fractionate _ _ = Nothing

    crossingTag
        :: Trig Double a
        => Earth Double
        -> SampleParams Double
        -> TaskZone Double
        -> (Cg.Fix, Cg.Fix)
        -> (Bool, Bool)
        -> Maybe InterpolatedFix

    crossingTag
        e
        sp
        z
        ( m@Cg.Fix{fix, time = t0, alt = RawAlt a0}
        , n@Cg.Fix{time = t1, alt = RawAlt a1}
        )
        inZones

        | inZones == (True, False) || inZones == (False, True) = do
            let pts = interpolate @Double @Double e sp z (fixToRadLL m) (fixToRadLL n)
            (LatLng (Lat xLat, Lng xLng), frac) <- fractionate @Double @Double e pts
            let a0' :: Quantity _ [u| m |] = MkQuantity (fromRational a0)
            let a1' :: Quantity _ [u| m |] = MkQuantity (fromRational a1)
            let MkQuantity xAlt = linearInterpolate frac a0' a1'

            let MkQuantity xLat' = convert xLat :: Quantity _ [u| deg |]
            let MkQuantity xLng' = convert xLng :: Quantity _ [u| deg |]
            let secs :: NominalDiffTime = t1 `diffUTCTime` t0
            let secs' :: NominalDiffTime = secs * (realToFrac frac)

            return
                InterpolatedFix
                    { fixFrac = fromIntegral fix + realToFrac frac
                    , time = secs' `addUTCTime` t0
                    , lat = RawLat . fromRational . toRational $ xLat'
                    , lng = RawLng . fromRational . toRational $ xLng'
                    , alt = RawAlt . fromRational . toRational $ xAlt
                    }

        | otherwise = Nothing

