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

import Data.UnitsOfMeasure (u, convert, fromRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Data.Bifunctor.Flip (Flip(..))

import Flight.LatLng (LatLng(..))
import Data.Number.RoundingFunctions (dpRound)

fromKms :: Fractional a => Quantity a [u| km |] -> TaskDistance a
fromKms q = TaskDistance (convert q)

newtype TaskDistance a =
    TaskDistance (Quantity a [u| m |])
    deriving (Eq, Ord)

instance Show (TaskDistance Rational) where
    show (TaskDistance d) = "d = " ++ show dbl
        where
            km = convert d :: Quantity Rational [u| km |]
            Flip rounded = dpRound 3 <$> Flip km
            dbl = fromRational' rounded :: Quantity Double [u| km |]

-- | The distance along a path of edges spanning vertices.
data PathDistance a =
    PathDistance
        { edgesSum :: TaskDistance a
        -- ^ The distance from the center of the first zone to the center of
        -- the last zone. An edge joins two vertices. These are summed to get
        -- the distance along the path that visits the vertices, each in turn.
        , vertices :: [ LatLng a [u| rad |] ]
        -- ^ The vertices that each edge spans.
        }
