module WireTypes.Lead
    ( LeadingArea(..)
    , LeadingCoefficient(..)
    , TrackLead(..)
    , showArea, showAreaDiff
    , showCoef, showCoefDiff
    ) where

import Text.Printf (printf)
import qualified Data.Text as T (Text, pack)
import Control.Applicative (empty)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON(..), Value(..))
import qualified Data.Text as T (unpack)

import WireTypes.Fraction (LeadingFraction)

newtype LeadingArea = LeadingArea Double
    deriving (Eq, Ord, Show, Generic)

newtype LeadingCoefficient = LeadingCoefficient Double
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

instance FromJSON LeadingArea where
    parseJSON x@(String _) = do
        s <- reverse . T.unpack <$> parseJSON x
        case s of
            's' : ' ' :'2' : '^' :  'm' : 'k' : ' ' : xs ->
                return . LeadingArea . read . reverse $ xs
            _ -> empty

    parseJSON _ = empty

data TrackLead =
    TrackLead
        { area :: LeadingArea
        , coef :: LeadingCoefficient
        , frac :: LeadingFraction
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

showArea :: LeadingArea -> T.Text
showArea (LeadingArea a) = T.pack $ printf "%.0f" a

showAreaDiff :: LeadingArea -> LeadingArea -> T.Text
showAreaDiff (LeadingArea expected) (LeadingArea actual)
    | f actual == f expected = "="
    | otherwise = f (actual - expected)
    where
        f = T.pack . printf "%+.0f"

showCoef :: LeadingCoefficient -> T.Text
showCoef (LeadingCoefficient lc) = T.pack $ printf "%.3f" lc

showCoefDiff :: LeadingCoefficient -> LeadingCoefficient -> T.Text
showCoefDiff (LeadingCoefficient expected) (LeadingCoefficient actual)
    | f actual == f expected = "="
    | otherwise = f (actual - expected)
    where
        f = T.pack . printf "%+.3f"
