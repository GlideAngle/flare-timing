module Flight.Geo
    ( LatLng(..)
    , Epsilon(..)
    , defEps
    , radToDeg
    , radToDegLL
    , earthRadius
    ) where

import Data.Ratio((%))
import qualified Data.Number.FixedFunctions as F

newtype LatLng = LatLng (Rational, Rational) deriving (Eq, Ord, Show)

newtype Epsilon = Epsilon Rational deriving (Eq, Ord, Show)

radToDeg :: Epsilon -> Rational -> Rational
radToDeg (Epsilon eps) x = x * (180 % 1) / F.pi eps

radToDegLL :: Epsilon -> LatLng -> LatLng
radToDegLL e (LatLng (lat, lng)) =
    LatLng (radToDeg e lat, radToDeg e lng)

defEps :: Epsilon
defEps = Epsilon $ 1 % 1000000000

earthRadius :: Rational
earthRadius = 6371000
