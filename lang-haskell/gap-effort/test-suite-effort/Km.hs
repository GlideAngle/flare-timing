module Km where

import Text.Printf (printf)
import Data.Maybe (fromMaybe)
import Data.UnitsOfMeasure (u, zero, unQuantity)
import Data.UnitsOfMeasure.Show (showQuantity)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import "flight-gap-allot" Flight.Score
    ( PilotDistance(..)
    , FlownMax(..)
    , bestDistance'
    )

type Km = Quantity Double [u| km |]

mkKm :: Double -> Quantity Double [u| km |]
mkKm = [u| km |]

distKms :: [Double] -> [PilotDistance Km]
distKms = fmap (PilotDistance . mkKm)

bd :: [PilotDistance Km] -> FlownMax Km
bd xs = fromMaybe (FlownMax zero) $ bestDistance' xs

showKm :: Km -> String
showKm = showQuantity

showKms :: [PilotDistance Km] -> String
showKms xs =
    go $ f <$> xs
    where
        f (PilotDistance x) = unQuantity x

        go ys
            | 1 <- length xs
            , y : _ <- ys = printf "[%7.3f]" y

            | 2 <- length xs
            , i : j : _ <- ys = printf "[%6.3f, %7.3f]" i j

            | length xs > 2
            , i : j : _ <- ys
            , k : _ <- reverse ys = printf "[%6.3f, %6.3f, ... %7.3f]" i j k

            | otherwise = show ys
