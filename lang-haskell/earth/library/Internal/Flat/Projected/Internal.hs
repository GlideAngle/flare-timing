{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Internal.Flat.Projected.Internal
    ( DistanceAzimuth(..)
    , pythagorean
    , zoneToProjectedEastNorth 
    , tooFar
    , _EN
    , _LLtoDMS
    ) where

import Prelude hiding (flip)
import Data.Functor.Identity (runIdentity)
import Control.Monad.Except (runExceptT)
import Data.UnitsOfMeasure (u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import qualified UTMRef as HC (UTMRef(..), toUTMRef, toLatLng)
import qualified LatLng as HC (LatLng(..), mkLatLng)
import qualified Datum as HC (wgs84Datum)

import Flight.Units ()
import Flight.Units.Angle (Angle(..))
import Flight.Units.DegMinSec (DMS(..))
import Flight.LatLng (LatLng(..), Lat(..), Lng(..), radToDegLL, degToRadLL)
import Flight.LatLng.Double (radToDeg, degToRad)
import Flight.Zone (Zone(..), center, realToFracZone)
import Flight.Distance (QTaskDistance, TaskDistance(..))

import qualified Internal.Sphere.PointToPoint.Double as H (distance, azimuthFwd, azimuthRev)

data DistanceAzimuth a =
    DistanceAzimuth
        { dist :: Quantity a [u| m |]
        , azFwd :: Quantity a [u| rad |]
        , azRev :: Quantity a [u| rad |]
        }
    deriving Show

tooFar :: Num a => QTaskDistance a [u| m |]
tooFar = TaskDistance [u| 20000000 m |]

zoneToProjectedEastNorth :: Real a => Zone a -> Either String HC.UTMRef
zoneToProjectedEastNorth z =
    runIdentity . runExceptT $ do
        xLL <- HC.mkLatLng lat lng 0 HC.wgs84Datum
        HC.toUTMRef xLL
    where
        cRad = center . realToFracZone $ z
        (LatLng (Lat qLat, Lng qLng)) = radToDegLL radToDeg cRad
        MkQuantity lat = qLat
        MkQuantity lng = plusMinusPi qLng

-- |
-- >>> HC.ellipsoid HC.wgs84Datum
-- [semi-major axis = 6378137.0, semi-minor axis = 6356752.3142]
--
-- >>> runIdentity . runExceptT $ checkLatLng 0 0
-- Right ((0°,0°),((166021.44317932962,0.0),('N',31)))
--
-- >>> runIdentity . runExceptT $ checkLatLng 0 180
-- Right ((0°,180°),((166021.4431793306,0.0),('N',1)))
--
-- >>> runIdentity . runExceptT $ checkLatLng 0 (negate 180)
-- Right ((0°,-180°),((166021.4431793306,0.0),('N',1)))
--
-- >>> runIdentity . runExceptT $ checkLatLng 0 181
-- Right ((0°,-179°),((277404.56033100287,0.0),('N',1)))
--
-- >>> runIdentity . runExceptT $ checkLatLng 0 (negate 181)
-- Right ((0°,179°),((722595.4396689972,0.0),('N',60)))
--
-- >>> runIdentity . runExceptT $ checkLatLng 45 0
-- Right ((45°,0°),((263553.9738663146,4987329.504902129),('T',31)))
--
-- >>> runIdentity . runExceptT $ checkLatLng (negate 45) 0
-- Right ((-45°,0°),((263553.9738663146,5012670.495097871),('G',31)))
--
-- >>> runIdentity . runExceptT $ checkLatLng 90 0
-- Left "Latitude (90.0) falls outside the UTM grid."
--
-- >>> runIdentity . runExceptT $ checkLatLng 85 0
-- Left "Latitude (85.0) falls outside the UTM grid."
--
-- >>> runIdentity . runExceptT $ checkLatLng 84 0
-- Right ((84°,0°),((465005.34493874514,9329005.182975996),('X',31)))
--
-- >>> runIdentity . runExceptT $ checkLatLng (negate 90) 0
-- Left "Latitude (-90.0) falls outside the UTM grid."
--
-- >>> runIdentity . runExceptT $ checkLatLng (negate 81) 0
-- Left "Latitude (-81.0) falls outside the UTM grid."
--
-- >>> runIdentity . runExceptT $ checkLatLng (negate 80) 0
-- Right ((-80°,0°),((441867.7848676922,1116915.0433355588),('C',31)))
--
-- Check the latitude bands, C .. X, each 8°. There are no bands I and O.
-- >>> partitionEithers $ runIdentity . runExceptT . checkLatZ <$> [-80.0,-72.0 .. 80.0]
-- ([],[(-80°,'C'),(-72°,'D'),(-64°,'E'),(-56°,'F'),(-48°,'G'),(-40°,'H'),(-32°,'J'),(-24°,'K'),(-16°,'L'),(-8°,'M'),(0°,'N'),(8°,'P'),(16°,'Q'),(24°,'R'),(32°,'S'),(40°,'T'),(48°,'U'),(56°,'V'),(64°,'W'),(72°,'X'),(80°,'X')])
--
-- Check the longitude bands, 1 .. 60, each 6°.
-- >>> partitionEithers $ runIdentity . runExceptT . checkLngZ <$> [-180.0,-174.0 .. 180.0]
-- ([],[(-180°,1),(-174°,2),(-168°,3),(-162°,4),(-156°,5),(-150°,6),(-144°,7),(-138°,8),(-132°,9),(-126°,10),(-120°,11),(-114°,12),(-108°,13),(-102°,14),(-96°,15),(-90°,16),(-84°,17),(-78°,18),(-72°,19),(-66°,20),(-60°,21),(-54°,22),(-48°,23),(-42°,24),(-36°,25),(-30°,26),(-24°,27),(-18°,28),(-12°,29),(-6°,30),(0°,31),(6°,32),(12°,33),(18°,34),(24°,35),(30°,36),(36°,37),(42°,38),(48°,39),(54°,40),(60°,41),(66°,42),(72°,43),(78°,44),(84°,45),(90°,46),(96°,47),(102°,48),(108°,49),(114°,50),(120°,51),(126°,52),(132°,53),(138°,54),(144°,55),(150°,56),(156°,57),(162°,58),(168°,59),(174°,60),(180°,1)])
--
-- >>> runIdentity . runExceptT $ pythag (0, 0) (0, 1)
-- Right (DistanceAzimuth {dist = [u| 111383.11715167353 m |], azFwd = [u| 0.0 rad |], azRev = [u| 3.141592653589793 rad |]})
--
-- >>> runIdentity . runExceptT $ pythag (0, 0) (0, -1)
-- Right (DistanceAzimuth {dist = [u| 111194.9266604789 m |], azFwd = [u| -1.5707963267948966 rad |], azRev = [u| 4.71238898038469 rad |]})
--
-- >>> runIdentity . runExceptT $ pythag (0, 0) (1, 0)
-- Right (DistanceAzimuth {dist = [u| 110682.84933219335 m |], azFwd = [u| 1.5703389856181167 rad |], azRev = [u| 4.71193163920791 rad |]})
--
-- >>> runIdentity . runExceptT $ pythag (0, 0) (-1, 0)
-- Right (DistanceAzimuth {dist = [u| 111194.92662668771 m |], azFwd = [u| -3.1415926535852594 rad |], azRev = [u| 3.1415926535943277 rad |]})
pythagorean :: HC.UTMRef -> HC.UTMRef -> Either String (DistanceAzimuth Double)
pythagorean
    x@HC.UTMRef
        { latZone = xLatZ
        , lngZone = xLngZ
        , easting = xE
        , northing = xN
        }
    y@HC.UTMRef
        { latZone = yLatZ
        , lngZone = yLngZ
        , easting = yE
        , northing = yN
        }
    | xLatZ == yLatZ && xLngZ == yLngZ =
        Right $
            DistanceAzimuth
                { dist = MkQuantity $ realToFrac d
                , azFwd = theta
                , azRev = rotate flip theta
                }
    | otherwise =
        either
            Left
            (\(x', y') ->
                let TaskDistance td = H.distance x' y' in
                case (H.azimuthFwd x' y', H.azimuthRev x' y') of
                    (Just fwd, Just rev) ->
                        Right $
                            DistanceAzimuth
                                { dist = td
                                , azFwd = fwd
                                , azRev = rev
                                }

                    _ -> Left "No azimuths")
            (runIdentity . runExceptT $ do
                HC.LatLng{latitude = xLat, longitude = xLng} <- HC.toLatLng x
                HC.LatLng{latitude = yLat, longitude = yLng} <- HC.toLatLng y
                let xDeg = LatLng (Lat $ MkQuantity xLat, Lng $ MkQuantity xLng)
                let yDeg = LatLng (Lat $ MkQuantity yLat, Lng $ MkQuantity yLng)
                return (degToRadLL degToRad xDeg, degToRadLL degToRad yDeg))

    where
        dN = yN - xN
        dE = yE - xE
        d = sqrt $ dN * dN + dE * dE

        theta :: Quantity _ [u| rad |]
        theta = normalize . MkQuantity $ atan2 dN dE

        flip :: Quantity _ [u| rad |]
        flip = convert [u| 180 deg |]

_EN :: HC.UTMRef -> ((Double, Double), (Char, Int))
_EN HC.UTMRef{easting = e, northing = n, latZone = latZ, lngZone = lngZ} =
    ((e, n), (latZ, lngZ))

_LL :: HC.LatLng -> (Double, Double)
_LL HC.LatLng{latitude = lat, longitude = lng} =
    (lat, lng)

_DMS :: (Double, Double) -> (DMS, DMS)
_DMS (lat, lng) =
    ( fromQuantity (MkQuantity lat :: Quantity Double [u| deg |])
    , fromQuantity (MkQuantity lng :: Quantity Double [u| deg |])
    )

_LLtoDMS :: HC.LatLng -> (DMS, DMS)
_LLtoDMS = _DMS . _LL

-- $setup
-- >>> import qualified Datum as HC
-- >>> import qualified LatLng as HC
-- >>> import Data.Either
-- >>> import Control.Monad.Except
--
-- >>> :{
-- checkLatLng lat lng = do
--     let qLng = MkQuantity lng :: Quantity _ [u| deg |]
--     let MkQuantity nLng = plusMinusPi qLng
--     ll <- HC.mkLatLng lat nLng 0 HC.wgs84Datum
--     u <- HC.toUTMRef ll
--     return $ (_LLtoDMS ll, _EN u)
-- :}
--
-- >>> :{
-- checkLatZ lat = do
--     ll <- HC.mkLatLng lat 0 0 HC.wgs84Datum
--     u <- HC.toUTMRef ll
--     return $ (fst $ _LLtoDMS ll, HC.latZone u)
-- :}
--
-- >>> :{
-- checkLngZ lng = do
--     let qLng = MkQuantity lng :: Quantity _ [u| deg |]
--     let MkQuantity nLng = plusMinusPi qLng
--     ll <- HC.mkLatLng 0 nLng 0 HC.wgs84Datum
--     u <- HC.toUTMRef ll
--     return $ (snd $ _LLtoDMS ll, HC.lngZone u)
-- :}
--
-- >>> :{
-- pythag (xLat, xLng) (yLat, yLng) = do
--     x <- HC.mkLatLng xLat xLng 0 HC.wgs84Datum
--     y <- HC.mkLatLng yLat yLng 0 HC.wgs84Datum
--     xEN <- HC.toUTMRef x
--     yEN <- HC.toUTMRef y
--     liftEither $ pythagorean xEN yEN
-- :}
