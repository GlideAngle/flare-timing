module Internal.Flat.Cylinder.Double (circumSample) where

import Data.Functor.Identity (runIdentity)
import Control.Monad.Except (runExceptT)
import Data.UnitsOfMeasure (u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import qualified UTMRef as HC (UTMRef(..), toLatLng)
import qualified LatLng as HC (LatLng(LatLng, latitude, longitude))

import Flight.Units ()
import Flight.LatLng (Lat(..), Lng(..), LatLng(..))
import Flight.Zone
    ( Zone(..)
    , QRadius
    , Radius(..)
    , Bearing(..)
    , ArcSweep(..)
    , center
    , radius
    , realToFracZone
    )
import Flight.Zone.Path (distancePointToPoint)
import Internal.Flat.PointToPoint.Double (distance)
import Flight.Distance (TaskDistance(..), PathDistance(..))
import Flight.Zone.Cylinder
    ( TrueCourse(..)
    , ZonePoint(..)
    , Tolerance(..)
    , SampleParams(..)
    , CircumSample
    , orbit
    , radial
    , point
    , sourceZone
    , sampleAngles
    )
import Internal.Flat.Projected.Internal (zoneToProjectedEastNorth)
import Flight.Earth.ZoneShape.Double (PointOnRadial, onLine)

fromHcLatLng :: HC.LatLng -> LatLng Double [u| rad |]
fromHcLatLng HC.LatLng{latitude, longitude} =
    LatLng (Lat $ convert lat, Lng $ convert lng)
    where
        lat :: Quantity Double [u| deg |]
        lat = MkQuantity latitude

        lng :: Quantity Double [u| deg |]
        lng = MkQuantity longitude

eastNorthToLatLng :: HC.UTMRef -> Either String HC.LatLng
eastNorthToLatLng = runIdentity . runExceptT . HC.toLatLng

-- |
--
-- >>> circumDeg (LatLng (Lat [u| -32.46363 deg |], Lng [u| 148.989 deg |])) (Radius [u| 286.27334927563106 m |]) [u| 332.30076790172313 deg |]
-- (-32.464787076769696°, 148.99172189459117°)
--
-- >>> circumDeg (LatLng (Lat [u| -32.46363 deg |], Lng [u| 148.989 deg |])) (Radius [u| 177.23328234645362 m |]) [u| 152.30076790172313 deg |]
-- (-32.462913617965945°, 148.9873148952786°)
--
-- >>> circumDeg (LatLng (Lat [u| 0.0 deg |], Lng [u| 0.0 deg |])) (Radius [u| 111383 m |]) [u| 0.0 deg |]
-- (0.0°, 0.9999989478488059°)
--
-- >>> circumDeg (LatLng (Lat [u| 0.0 deg |], Lng [u| 0.0 deg |])) (Radius [u| 111383 m |]) [u| 180.0 deg |]
-- (1.2310722351314232e-16°, -0.9990789538347999°)
--
-- >>> circumDeg (LatLng (Lat [u| 0.0 deg |], Lng [u| 0.0 deg |])) (Radius [u| 111383 m |]) [u| 90.0 deg |]
-- (1.0063254069690941°, -4.601156388384097e-4°)
--
-- >>> circumDeg (LatLng (Lat [u| 0.0 deg |], Lng [u| 0.0 deg |])) (Radius [u| 111383 m |]) [u| -90.0 deg |]
-- (-1.0063254069690941°, -4.601156388384097e-4°)
circum
    :: Real a
    => LatLng a [u| rad |]
    -> QRadius a [u| m |]
    -> TrueCourse a
    -> LatLng Double [u| rad |]
circum xLL r tc =
    case circumEN xLL r tc of
        Left s -> error s
        Right yEN ->
            case eastNorthToLatLng yEN of
                Left s -> error s
                Right yLL -> fromHcLatLng yLL

circumEN
    :: Real a
    => LatLng a [u| rad |]
    -> QRadius a [u| m |]
    -> TrueCourse a
    -> Either String HC.UTMRef
circumEN xLL r tc =
    translate r tc <$> zoneToProjectedEastNorth (Point xLL)

-- |
-- >>> runIdentity . runExceptT $ txE (0, 177) 0
-- Right ((0°,177°),((500000.0,0.0),('N',60)))
--
-- >>> runIdentity . runExceptT $ txE (0, 177) 333360
-- Right ((0°,179°59'40.01582844998438''),((833360.0,0.0),('N',60)))
--
-- >>> runIdentity . runExceptT $ txE (0, 177) 333978
-- Right ((0°,179°59'59.982009971051866''),((833978.0,0.0),('N',60)))
--
-- >>> runIdentity . runExceptT $ txE (0, 177) 333979
-- Right ((0°,180°0'1.4317625561943714e-2''),((833979.0,0.0),('N',60)))
--
-- >>> runIdentity . runExceptT $ txN (0, 177) 0
-- Right ((0°,177°),((500000.0,0.0),('N',60)))
--
-- >>> runIdentity . runExceptT $ txN (0, 177) (negate 1)
-- Right ((0.032570'',177°),((500000.0,-1.0),('N',60)))
--
-- >>> runIdentity . runExceptT $ txN (0, 177) 5000000
-- Right ((45°9'12.517858235586345'',177°),((500000.0,5000000.0),('N',60)))
--
-- >>> runIdentity . runExceptT $ txN (0, 177) (negate 5000000)
-- Right ((-45°9'12.517858235586345'',177°),((500000.0,-5000000.0),('N',60)))
--
-- >>> runIdentity . runExceptT $ txN (0, 177) 4982950.4002808
-- Right ((44°59'59.99999999710951'',177°),((500000.0,4982950.4002808),('N',60)))
--
-- >>> runIdentity . runExceptT $ txN (84, 0) 1
-- Left "Latitude (84.00000894932786) is invalid. Converting from UTM, it must be between -80.0 and 84.0 inclusive."
--
-- >>> runIdentity . runExceptT $ txN (84, 0) (negate 1)
-- Right ((83°59'59.96781120495939'',0.016017''),((465005.34493874514,9329004.182975996),('X',31)))
--
-- >>> runIdentity . runExceptT $ txN (-80, 0) 1
-- Right ((-79°59'59.96781219908257'',0.009534''),((441867.7848676922,1116916.0433355588),('C',31)))
--
-- >>> runIdentity . runExceptT $ txN (-80, 0) (negate 1)
-- Left "Latitude (-80.00000895308122) is invalid. Converting from UTM, it must be between -80.0 and 84.0 inclusive."
translate
    :: Real a
    => QRadius a [u| m |]
    -> TrueCourse a
    -> HC.UTMRef
    -> HC.UTMRef
translate (Radius (MkQuantity rRadius)) (TrueCourse (MkQuantity rtc)) x =
    HC.UTMRef
        (xE + dE)
        (xN + dN)
        (HC.latZone x)
        (HC.lngZone x)
        (HC.datum x)
    where
        xE :: Double
        xE = HC.easting x

        xN :: Double
        xN = HC.northing x

        rRadius' :: Double
        rRadius' = realToFrac rRadius

        rtc' :: Double
        rtc' = realToFrac rtc

        dE :: Double
        dE = rRadius' * cos rtc'

        dN :: Double
        dN = rRadius' * sin rtc'

-- | Generates a pair of lists, the lat/lng of each generated point and its
-- distance from the center. It will generate 'samples' number of such points
-- that should lie close to the circle. The difference between the distance to
-- the origin and the radius should be less than the 'tolerance'.
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

        getClose' = getClose zone' ptCenter limitRadius spTolerance

        mkLinePt :: PointOnRadial
        mkLinePt _ (Bearing b) rLine = circumR rLine $ TrueCourse b

getClose
    :: Zone Double
    -> LatLng Double [u| rad |] -- ^ The center point.
    -> Double -- ^ The limit radius.
    -> Tolerance Double
    -> Int -- ^ How many tries.
    -> QRadius Double [u| m |] -- ^ How far from the center.
    -> (TrueCourse Double -> LatLng Double [u| rad |]) -- ^ A point from the origin on this radial
    -> TrueCourse Double -- ^ The true course for this radial.
    -> (ZonePoint Double, TrueCourse Double)
getClose zone' ptCenter limitRadius spTolerance trys yr@(Radius (MkQuantity offset)) f x@(TrueCourse tc)
    | trys <= 0 = (zp', x)
    | unTolerance spTolerance <= 0 = (zp', x)
    | limitRadius <= unTolerance spTolerance = (zp', x)
    | otherwise =
        case d `compare` limitRadius of
             EQ ->
                 (zp', x)

             GT ->
                 let offset' =
                         offset - (d - limitRadius) * 105 / 100

                     f' =
                         circumR (Radius (MkQuantity $ limitRadius + offset'))

                 in
                     getClose
                         zone'
                         ptCenter
                         limitRadius
                         spTolerance
                         (trys - 1)
                         (Radius (MkQuantity offset'))
                         f'
                         x

             LT ->
                 if d > (limitRadius - unTolerance spTolerance)
                 then (zp', x)
                 else
                     let offset' =
                             offset + (limitRadius - d) * 94 / 100

                         f' =
                             circumR (Radius (MkQuantity $ limitRadius + offset'))

                     in
                         getClose
                             zone'
                             ptCenter
                             limitRadius
                             spTolerance
                             (trys - 1)
                             (Radius (MkQuantity offset'))
                             f'
                             x
    where
        circumR = circum ptCenter

        y = f x

        zp' :: ZonePoint Double
        zp' =
            ZonePoint
                { sourceZone = realToFracZone zone'
                , point = y
                , radial = Bearing tc
                , orbit = yr
                }

        (TaskDistance (MkQuantity d)) =
            edgesSum
            $ distancePointToPoint
                distance
                (realToFracZone <$> [Point ptCenter, Point y])

-- $setup
-- >>> import Data.UnitsOfMeasure ((*:), u, convert)
-- >>> import qualified UTMRef as HC
-- >>> import qualified LatLng as HC
-- >>> import qualified Datum as HC
-- >>> import Flight.LatLng (radToDegLL, degToRadLL)
-- >>> import Internal.Flat.Projected.Internal (_EN, _LLtoDMS)
--
-- >>> :{
-- circumDeg
--    :: RealFrac a
--    => LatLng a [u| deg |]
--    -> QRadius a [u| m |]
--    -> (Quantity a [u| deg |])
--    -> LatLng Double [u| deg |]
-- circumDeg ll r tc =
--     radToDegLL convert $ circum (degToRadLL convert ll) r (TrueCourse ((convert tc) :: Quantity _ [u| rad |]))
-- :}
--
-- >>> :{
-- tx (xLat, xLng) (dN, dE) = do
--     x <- HC.mkLatLng xLat xLng 0 HC.wgs84Datum
--     en@HC.UTMRef{easting = e, northing = n} <- HC.toUTMRef x
--     let en' = en{HC.easting = e + dE, HC.northing = n + dN}
--     y <- HC.toLatLng en'
--     return $ (_LLtoDMS y, _EN en')
-- :}
--
-- >>> :{
-- txN (xLat, xLng) dN = do
--     x <- HC.mkLatLng xLat xLng 0 HC.wgs84Datum
--     en@HC.UTMRef{easting = e, northing = n} <- HC.toUTMRef x
--     let en'@HC.UTMRef{easting = e', northing = n', latZone, lngZone} = en{HC.northing = n + dN}
--     y <- HC.toLatLng en'
--     return $ (_LLtoDMS y, ((e', n'), (latZone, lngZone)))
-- :}
--
-- >>> :{
-- txE (xLat, xLng) dE = do
--     x <- HC.mkLatLng xLat xLng 0 HC.wgs84Datum
--     en@HC.UTMRef{easting = e, northing = n} <- HC.toUTMRef x
--     let en'@HC.UTMRef{easting = e', northing = n', latZone, lngZone} = en{HC.easting = e + dE}
--     y <- HC.toLatLng en'
--     return $ (_LLtoDMS y, ((e', n'), (latZone, lngZone)))
-- :}
