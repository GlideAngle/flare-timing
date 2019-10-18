module WireTypes.Lead
    ( LeadingArea(..)
    , LeadingCoefficient(..)
    , TrackLead(..)
    ) where

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
