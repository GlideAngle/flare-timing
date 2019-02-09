module Flight.Gap.Place.Arrival (ArrivalPlacing(..)) where

import Control.Applicative (empty)
import qualified Data.Text as T
import Data.Aeson (Value(..), ToJSON(..), FromJSON(..))

-- | A 1-based rank of the pilot arrival at goal, 1st in is 1, 2nd is 2 etc.
data ArrivalPlacing
    = ArrivalPlacing Integer -- ^ The nth place
    | ArrivalPlacingEqual Integer Integer -- ^ This placing shared with n other pilots
    deriving (Eq, Ord, Show)

instance ToJSON ArrivalPlacing where
    toJSON (ArrivalPlacing x) = String . T.pack $ show x
    toJSON (ArrivalPlacingEqual x y) = String . T.pack $ show (x, y) ++ "="

instance FromJSON ArrivalPlacing where
    parseJSON x@(String _) = do
        s <- T.unpack <$> parseJSON x
        case reverse s of
            '=' : rest ->
                return . uncurry ArrivalPlacingEqual . read . reverse $ rest

            _ ->
                return . ArrivalPlacing . read $ s

    parseJSON _ = empty