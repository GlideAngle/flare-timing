{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module Flight.Distance
    ( TaskDistance(..)
    , PathDistance(..)
    , fromKms
    ) where

import Data.UnitsOfMeasure (u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..), fromRational')
import Data.Bifunctor.Flip (Flip(..))

import Flight.LatLng (LatLng(..))
import Data.Number.RoundingFunctions (dpRound)

fromKms :: Quantity Rational [u| km |] -> TaskDistance
fromKms q = TaskDistance (convert q)

newtype TaskDistance =
    TaskDistance (Quantity Rational [u| m |])
    deriving (Eq, Ord)

instance Show TaskDistance where
    show (TaskDistance d) = "d = " ++ show dbl
        where
            km = convert d :: Quantity Rational [u| km |]
            Flip rounded = dpRound 3 <$> Flip km
            dbl = fromRational' rounded :: Quantity Double [u| km |]

instance {-# OVERLAPPING #-} Show [TaskDistance] where
    show = showDistances

showDistances :: [TaskDistance] -> String
showDistances xs =
    show (f <$> xs) ++ " km"
    where
        f (TaskDistance d) = show dbl
            where
                km = convert d :: Quantity Rational [u| km |]
                Flip rounded = dpRound 3 <$> Flip km
                (MkQuantity dbl) = fromRational' rounded :: Quantity Double [u| km |]


-- | The distance along a path of edges spanning vertices.
data PathDistance =
    PathDistance
        { edgesSum :: TaskDistance
        -- ^ The distance from the center of the first zone to the center of
        -- the last zone. An edge joins two vertices. These are summed to get
        -- the distance along the path that visits the vertices, each in turn.
        , vertices :: [ LatLng [u| rad |] ]
        -- ^ The vertices that each edge spans.
        }
