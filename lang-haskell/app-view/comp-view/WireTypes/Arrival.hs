module WireTypes.Arrival
    ( ArrivalPlacing(..)
    , TrackArrival(..)
    ) where

import Control.Applicative (empty)
import GHC.Generics (Generic)
import Data.Aeson (Value(..), FromJSON(..))
import qualified Data.Text as T (unpack)

import WireTypes.Fraction (ArrivalFraction)

data ArrivalPlacing
    = ArrivalPlacing Integer
    | ArrivalPlacingEqual Integer Integer
    deriving (Eq, Ord, Show)

instance FromJSON ArrivalPlacing where
    parseJSON x@(String _) = do
        s <- T.unpack <$> parseJSON x
        case reverse s of
            '=' : pair ->
                case reverse pair of
                    xy@('(' : _) ->
                        let (m, n) = read xy in return $ ArrivalPlacingEqual m n

                    _ -> empty

            _ ->
                return . ArrivalPlacing . read $ s

    parseJSON _ = empty

data TrackArrival =
    TrackArrival
        { rank :: ArrivalPlacing
        , frac :: ArrivalFraction
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)
