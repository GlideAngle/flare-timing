module WireTypes.Arrival
    ( ArrivalFraction(..)
    , ArrivalPlacing(..)
    , TrackArrival(..)
    ) where

import Control.Applicative (empty)
import GHC.Generics (Generic)
import Data.Aeson (Value(..), FromJSON(..))
import qualified Data.Text as T (unpack)
import Data.List.Split (splitOn)

newtype ArrivalFraction = ArrivalFraction Double
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

data ArrivalPlacing
    = ArrivalPlacing Integer
    | ArrivalPlacingEqual Integer Integer
    deriving (Eq, Ord, Show)

instance FromJSON ArrivalPlacing where
    parseJSON x@(String _) = do
        s <- T.unpack <$> parseJSON x
        case reverse s of
            '=' : digits ->
                case splitOn " " $ reverse digits of
                    [m, n] ->
                        let m' = read m
                            n' = read n
                        in return $ ArrivalPlacingEqual m' n'

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
