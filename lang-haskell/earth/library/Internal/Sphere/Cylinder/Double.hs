module Internal.Sphere.Cylinder.Double (circumSample, direct) where

import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.LatLng (Lat(..), Lng(..), LatLng(..))
import Flight.Zone
    ( Zone(..)
    , QRadius
    , Radius(..)
    , Bearing(..)
    , ArcSweep(..)
    , center
    , radius
    , realToFracLatLng
    )
import Flight.Zone.Cylinder
    ( TrueCourse(..)
    , ZonePoint(..)
    , SampleParams(..)
    , CircumSample
    , orbit
    , radial
    , point
    , sourceZone
    , sampleAngles
    )
import Flight.Earth.Ellipsoid (GeodeticDirect(..))
import Flight.Geodesy (DirectProblem(..), DirectSolution(..))
import Flight.Earth.Sphere (earthRadius)
import Flight.Earth.ZoneShape.Double (PointOnRadial, onLine)
import qualified Internal.Sphere.PointToPoint.Double as H (distance)
import Internal.CylinderOutline.Double (getClose)

direct'
    :: RealFloat a
    => DirectProblem
        (LatLng a [u| rad |])
        (TrueCourse a)
        (QRadius a [u| m |])
    -> DirectSolution
        (LatLng a [u| rad |])
        (TrueCourse a)
direct'
    DirectProblem
        { x = LatLng (Lat (MkQuantity lat), Lng (MkQuantity lng))
        , α₁ = TrueCourse (MkQuantity tc)
        , s = Radius (MkQuantity r)
        } =
    DirectSolution
        { y = LatLng (Lat (MkQuantity φ2), Lng (MkQuantity λ2))
        -- TODO: Give the reverse azimuth with Haversines for the direct
        -- geodesy solution.
        , α₂ = Nothing
        }
    where
        φ1 = realToFrac lat
        λ1 = realToFrac lng
        θ = realToFrac tc

        δ = let Radius (MkQuantity bigR) = earthRadius in realToFrac r / bigR

        φ2 = asin $ sin φ1 * cos δ + cos φ1 * sin δ * cos θ
        λ2 = λ1 + atan2 (sin θ * sin δ * cos φ1) (cos δ - sin φ1 * sin φ2)

direct
    :: RealFloat a
    => DirectProblem
        (LatLng a [u| rad |])
        (TrueCourse a)
        (QRadius a [u| m |])
    -> GeodeticDirect
        (DirectSolution
            (LatLng a [u| rad |])
            (TrueCourse a)
        )
direct prob = GeodeticDirect $ direct' prob


-- | Using a method from the
-- <http://www.edwilliams.org/avform.htm#LL Aviation Formulary>
-- a point on a cylinder wall is found by going out to the distance of the
-- radius on the given radial true course 'rtc'.
-- <https://www.movable-type.co.uk/scripts/latlong.html Movable Type Scripts>
-- has working examples in Javascript backed HTML pages.
--
-- >>> circumDeg (LatLng (Lat [u| -32.46363 deg |], Lng [u| 148.989 deg |])) (Radius [u| 286.27334927563106 m |]) [u| 332.30076790172313 deg |]
-- (-32.46135051411138°, 148.98758167886174°)
--
-- >>> circumDeg (LatLng (Lat [u| -32.46363 deg |], Lng [u| 148.989 deg |])) (Radius [u| 177.23328234645362 m |]) [u| 152.30076790172313 deg |]
-- (-32.46504123330422°, 148.9898781257936°)
circum
    :: RealFloat a
    => LatLng a [u| rad |]
    -> QRadius a [u| m |]
    -> TrueCourse a
    -> LatLng Double [u| rad |]
circum x s tc =
    let prob = DirectProblem {x = x, α₁ = tc, s = s}
        DirectSolution{y} = direct' prob
    in realToFracLatLng y

-- | Generates a pair of lists, the lat/lng of each generated point
-- and its distance from the center. It will generate 'samples' number of such
-- points that should lie close to the circle. The difference between
-- the distance to the origin and the radius should be less han the 'tolerance'.
--
-- The points of the compass are divided by the number of samples requested.
circumSample :: CircumSample Double
circumSample SampleParams{..} arcSweep@(ArcSweep (Bearing (MkQuantity bearing))) arc0 zoneM zoneN
    | bearing < 0 || bearing > 2 * pi = error "Arc sweep must be in the range 0..2π radians."
    | otherwise =
        case spSamples of
            [] -> error "Empty list of sample numbers."
            sp0 : _ ->
                let (θ, xs) = sampleAngles pi sp0 arcSweep arc0 zoneM zoneN

                    ys :: ([ZonePoint Double], [TrueCourse Double])
                    ys = unzip $ getClose' 10 (Radius (MkQuantity 0)) (circumR r) <$> xs

                in
                    case (zoneM, zoneN) of
                        (Nothing, _) -> ys
                        (Just _, Point{}) -> ys
                        (Just _, Vector{}) -> ys
                        (Just _, Cylinder{}) -> ys
                        (Just _, Conical{}) -> ys
                        (Just _, Line{}) -> onLine mkLinePt θ ys
                        (Just _, Circle{}) -> ys
                        (Just _, SemiCircle{}) -> ys
    where
        zone' :: Zone Double
        zone' =
            case arc0 of
              Nothing -> zoneN
              Just ZonePoint{..} -> sourceZone

        r :: QRadius Double [u| m |]
        r@(Radius (MkQuantity limitRadius)) = radius zone'

        ptCenter = center zone'
        circumR = circum ptCenter

        getClose' = getClose H.distance circum zone' ptCenter limitRadius spTolerance

        mkLinePt :: PointOnRadial
        mkLinePt _ (Bearing b) rLine = circumR rLine $ TrueCourse b

-- $setup
-- >>> :set -XTemplateHaskell
-- >>> :set -XQuasiQuotes
-- >>> :set -XDataKinds
-- >>> :set -XFlexibleContexts
-- >>> :set -XFlexibleInstances
-- >>> :set -XMultiParamTypeClasses
-- >>> :set -XScopedTypeVariables
-- >>> :set -XTypeOperators
-- >>> :set -XTypeFamilies
-- >>> :set -XUndecidableInstances
-- >>> :set -fno-warn-partial-type-signatures
--
-- >>> import Data.UnitsOfMeasure ((*:), u, convert)
-- >>> import Flight.LatLng (radToDegLL, degToRadLL)
--
-- >>> :{
-- circumDeg
--    :: RealFloat a
--    => LatLng a [u| deg |]
--    -> QRadius a [u| m |]
--    -> (Quantity a [u| deg |])
--    -> LatLng Double [u| deg |]
-- circumDeg ll r tc =
--     radToDegLL convert $ circum (degToRadLL convert ll) r (TrueCourse ((convert tc) :: Quantity _ [u| rad |]))
-- :}
