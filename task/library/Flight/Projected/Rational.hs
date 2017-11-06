{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}

{-# LANGUAGE PartialTypeSignatures #-}

module Flight.Projected.Rational (costEastNorth) where

import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Zone (Zone(..), center, toRationalZone)
import Flight.Distance (TaskDistance(..), PathDistance(..))
import Flight.Units ()
import Flight.Projected.Internal (pythagorean , zoneToProjectedEastNorth, tooFar)

-- | The task distance returned is for the projected UTM plane with
-- eastings and northings. If you need to calculate the distance in sperical
-- coordinates, the latitude and longitude of each vertex of the path can be
-- used to work that out.
costEastNorth :: Real a
              => Zone a
              -> Zone a
              -> PathDistance Rational

costEastNorth x@(Point _) y@(Point _) =
    PathDistance { edgesSum = d'
                 , vertices = center . toRationalZone <$> [x, y]
                 }
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
    PathDistance { edgesSum = tooFar
                 , vertices = center . toRationalZone <$> [x, y]
                 }
