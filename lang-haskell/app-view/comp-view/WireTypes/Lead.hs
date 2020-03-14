module WireTypes.Lead
    ( LeadingArea(..)
    , LeadingCoefficient(..)
    , TrackLead(..)
    , RawLeadingArea(..)
    , EssTime(..)
    , showArea, showAreaDiff
    , showCoef, showCoefDiff
    , nullArea
    ) where

import Text.Printf (printf)
import qualified Data.Text as T (Text, pack)
import Data.List
import Data.List.Split
import Control.Applicative (empty)
import Data.Foldable (asum)
import GHC.Generics (Generic)
import Data.Aeson ((.:), FromJSON(..), Value(..), withObject)
import qualified Data.Text as T (unpack)

import WireTypes.Fraction (LeadingFraction)
import WireTypes.Route (TaskDistance(..))

newtype EssTime = EssTime Double
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

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
showArea (LeadingArea a) =
    T.pack . f $ printf "%.0f" a
    where
        -- SEE: https://stackoverflow.com/questions/3752898/haskell-format-number-with-commas
        f x =
            let (h, t') = case break (== '.') x of ([], t) -> (t, []); ht -> ht
                h' = reverse . intercalate "," . chunksOf 3 $ reverse h
            in
                h' ++ t'

showAreaDiff :: LeadingArea -> LeadingArea -> T.Text
showAreaDiff (LeadingArea expected) (LeadingArea actual)
    | f actual == f expected = "="
    | expected == 0 = "∞"
    | otherwise = f (actual / expected - 1.0)
    where
        f = T.pack . printf "%+.4f"

showCoef :: LeadingCoefficient -> T.Text
showCoef (LeadingCoefficient lc) = T.pack $ printf "%.3f" lc

showCoefDiff :: LeadingCoefficient -> LeadingCoefficient -> T.Text
showCoefDiff (LeadingCoefficient expected) (LeadingCoefficient actual)
    | f actual == f expected = "="
    | otherwise = f (actual - expected)
    where
        f = T.pack . printf "%+.3f"

nullArea :: RawLeadingArea
nullArea =
    RawLeadingArea
        { leadAllDown = Nothing
        , raceDistance = Nothing
        , distanceTime = []
        , distanceTimeAfterDown = []
        , distanceTimeBeforeStart = []
        }

data RawLeadingArea =
    RawLeadingArea
        { leadAllDown :: Maybe EssTime
        , raceDistance :: Maybe TaskDistance
        -- ^ The distance of the speed section in km.
        , distanceTime :: [[Double]]
        -- ^ Pairs of (distance, lead time) in (km, s).
        , distanceTimeAfterDown :: [[Double]]
        , distanceTimeBeforeStart :: [[Double]]
        }
    deriving (Eq, Ord, Generic)

instance FromJSON RawLeadingArea where
    parseJSON = withObject "RawLeadingArea" $ \o ->
        asum
            [ do
                ld :: Maybe EssTime <- o .: "lead-all-down"
                rd :: Maybe TaskDistance <- o .: "race-distance"
                dt :: [[Double]] <- o .: "distance-time"
                ad :: [[Double]] <- o .: "distance-time-after-down"
                bs :: [[Double]] <- o .: "distance-time-before-start"
                return
                    RawLeadingArea
                        { leadAllDown = ld
                        , raceDistance = rd
                        , distanceTime = dt
                        , distanceTimeAfterDown = ad
                        , distanceTimeBeforeStart = bs
                        }
            ]
