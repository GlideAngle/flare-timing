module Flight.Gap.Place.Task (TaskPlacing(..)) where

import Control.Applicative (empty)
import qualified Data.Text as T
import Data.Aeson (Value(..), ToJSON(..), FromJSON(..))

data TaskPlacing
    = TaskPlacing Integer
    | TaskPlacingEqual Integer
    deriving (Eq, Ord, Show)

instance ToJSON TaskPlacing where
    toJSON (TaskPlacing x) = String . T.pack $ show x
    toJSON (TaskPlacingEqual x) = String . T.pack $ show x ++ "="

instance FromJSON TaskPlacing where
    parseJSON x@(String _) = do
        s <- T.unpack <$> parseJSON x
        case reverse s of
            '=' : digits ->
                return . TaskPlacingEqual . read . reverse $ digits

            _ ->
                return . TaskPlacing . read $ s

    parseJSON _ = empty