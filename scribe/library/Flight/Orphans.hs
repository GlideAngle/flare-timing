{-# OPTIONS_GHC -fno-warn-orphans #-}

module Flight.Orphans where

import Data.Ratio
import qualified Data.Text as T (pack)
import Data.Functor.Contravariant

import Dhall

import Data.UnitsOfMeasure
import Data.UnitsOfMeasure.Internal

import Flight.Distance
import Flight.LatLng.Raw
import Flight.EastNorth
import Flight.Route

deriving instance Inject a => Inject (OptimalRoute a)
deriving instance Inject TrackLine
deriving instance Inject ProjectedTrackLine
deriving instance Inject PlanarTrackLine
deriving instance Inject TaskTrack
deriving instance Inject RawLat
deriving instance Inject RawLng
deriving instance Inject RawLatLng
deriving instance Inject UtmZone
deriving instance Inject EastingNorthing

deriving instance (u ~ [u| m |], Inject a) => Inject (QTaskDistance a u)

-- TODO: Remove Inject Char
instance Inject Char where
    injectWith =
        fmap (contramap $ \c -> T.pack [c]) injectWith

instance Inject (Ratio Integer) where
    injectWith =
        fmap (contramap $ \x -> let y :: Double = fromRational x in y) injectWith

instance Inject a => Inject (Quantity a u) where
    injectWith =
        fmap (contramap $ \(MkQuantity x) -> x) injectWith
