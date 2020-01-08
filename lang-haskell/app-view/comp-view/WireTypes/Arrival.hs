module WireTypes.Arrival
    ( ArrivalPlacing(..)
    , ArrivalLag(..)
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

newtype ArrivalLag = ArrivalLag Double
    deriving (Eq, Ord, Show)

instance FromJSON ArrivalLag where
    parseJSON x@(String _) = do
        s <- reverse . T.unpack <$> parseJSON x
        case s of
            'h' : ' ' : xs -> return . ArrivalLag . read . reverse $ xs
            _ -> empty
    parseJSON _ = empty

data TrackArrival =
    TrackArrival
        { rank :: ArrivalPlacing
        , lag :: ArrivalLag
        , frac :: ArrivalFraction
        }
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)
