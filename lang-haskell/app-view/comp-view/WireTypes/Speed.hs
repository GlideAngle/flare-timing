module WireTypes.Speed
    ( PilotTime(..)
    , TrackSpeed(..)
    ) where

import Control.Applicative (empty)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON(..), Value(..))
import qualified Data.Text as T (unpack)

import WireTypes.Fraction (SpeedFraction)

newtype PilotTime = PilotTime Double
    deriving (Eq, Ord, Show)

instance FromJSON PilotTime where
    parseJSON x@(String _) = do
        s <- reverse . T.unpack <$> parseJSON x
        case s of
            'h' : ' ' : xs -> return . PilotTime . read . reverse $ xs
            _ -> empty
    parseJSON _ = empty

data TrackSpeed =
    TrackSpeed
        { time :: PilotTime
        , frac :: SpeedFraction
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)
