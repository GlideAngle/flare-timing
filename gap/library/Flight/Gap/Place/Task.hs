module Flight.Gap.Place.Task (TaskPlacing(..)) where

import Control.Applicative (empty)
import qualified Data.Text as T
import Data.Aeson (Value(..), ToJSON(..), FromJSON(..))

data TaskPlacing
    = TaskPlacing Integer
    | TaskPlacingEqual Integer
    deriving (Eq, Show)

instance Ord TaskPlacing where
    compare (TaskPlacing a) (TaskPlacing b) = compare a b
    compare (TaskPlacingEqual a) (TaskPlacingEqual b) = compare a b
    compare (TaskPlacing a) (TaskPlacingEqual b) = compare a b
    compare (TaskPlacingEqual a) (TaskPlacing b) = compare a b

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
