module ServeArea (RawLeadingArea(..)) where

import GHC.Generics (Generic)
import Data.Maybe (catMaybes)
import Data.Aeson (ToJSON(..))

import Flight.Track.Time (TickRow(..), LeadTick(..))

newtype RawLeadingArea = RawLeadingArea [TickRow]
    deriving (Eq, Ord, Generic)

instance ToJSON RawLeadingArea where
    toJSON (RawLeadingArea xs) =
        toJSON . catMaybes $ mkRaw <$> xs

mkRaw :: TickRow -> Maybe [Double]
mkRaw TickRow{tickLead, togo}
    | tickLead == Nothing = Nothing
    | tickLead < Just 0 = Nothing
    | otherwise = (\(LeadTick t) -> [t, togo]) <$> tickLead
