{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module Flight.Projected
    ( costEastNorth
    , zoneToProjectedEastNorth
    ) where

import Data.Functor.Identity (runIdentity)
import Control.Monad.Except (runExceptT)
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import qualified UTMRef as HC (UTMRef(..), toUTMRef)
import qualified LatLng as HC (mkLatLng)
import qualified Datum as HC (wgs84Datum)

import Flight.Zone (Zone(..), center)
import Flight.Distance (TaskDistance(..), PathDistance(..))
import Flight.Units ()
import Flight.LatLng (LatLng(..), Lat(..), Lng(..), radToDegLL, defEps)

zoneToProjectedEastNorth :: Zone a -> Either String HC.UTMRef

zoneToProjectedEastNorth (Point x) = do
    xLL <- runIdentity . runExceptT $ HC.mkLatLng xLat' xLng' 0 HC.wgs84Datum
    runIdentity . runExceptT $ HC.toUTMRef xLL
    where
        (LatLng (Lat (MkQuantity xLat), Lng (MkQuantity xLng))) =
            radToDegLL defEps x

        xLat' :: Double
        xLat' = fromRational xLat :: Double

        xLng' :: Double
        xLng' = fromRational xLng :: Double

tooFar :: TaskDistance
tooFar = TaskDistance [u| 20000000 m |]

-- | The task distance returned is for the projected UTM plane with
-- eastings and northings. If you need to calculate the distance in sperical
-- coordinates, the latitude and longitude of each vertex of the path can be
-- used to work that out.
costEastNorth :: Zone a -> Zone a -> PathDistance

costEastNorth x@(Point _) y@(Point _) =
    PathDistance { edgesSum = d', vertices = center <$> [x, y] }
    where
        d' =
            case (zoneToProjectedEastNorth x, zoneToProjectedEastNorth y) of
                (Right xLL, Right yLL) ->
                    TaskDistance dm
                    where
                        d :: Double
                        d = pythagorean xLL yLL

                        dm :: Quantity Rational [u| m |]
                        dm = MkQuantity $ toRational d

                _ -> tooFar

costEastNorth x y =
    PathDistance { edgesSum = tooFar, vertices = center <$> [x, y] }

pythagorean :: HC.UTMRef -> HC.UTMRef -> Double
pythagorean x y =
    sqrt $ dN * dN + dE * dE
    where
        dN = yN - xN
        dE = yE - xE

        xN = HC.northing x
        yN = HC.northing y

        xE = HC.easting x
        yE = HC.easting y
