module Flight.Geodesy.Math
    ( EarthMath(..)
    , Projection(..)
    , EarthModel(..)
    ) where

import qualified Data.Text as T (pack)
import Control.Applicative (empty)
import GHC.Generics (Generic)
import Data.Aeson
    ( Value(..), ToJSON(..), FromJSON(..), Options(..), SumEncoding(..)
    , genericToJSON, genericParseJSON, defaultOptions
    )
import Data.UnitsOfMeasure (u)

import Flight.Units ()
import Flight.Zone (QRadius)
import Flight.Earth.Ellipsoid (Ellipsoid(..))

data EarthMath
    = Pythagorus
    | Haversines
    | Vincenty
    | AndoyerLambert
    | ForsytheAndoyerLambert
    | FsAndoyer
    deriving (Eq, Ord, Show)

instance ToJSON EarthMath where
    toJSON em@Pythagorus = toJSON . String . T.pack $ show em
    toJSON em@Haversines = toJSON . String . T.pack $ show em
    toJSON em@Vincenty = toJSON . String . T.pack $ show em
    toJSON AndoyerLambert = toJSON $ String "Andoyer-Lambert"
    toJSON ForsytheAndoyerLambert = toJSON $ String "Forsythe-Andoyer-Lambert"
    toJSON FsAndoyer = toJSON $ String "FS-Andoyer"

instance FromJSON EarthMath where
    parseJSON o@(String _) = do
        s :: String <- parseJSON o
        case s of
            "Pythagorus" -> return Pythagorus
            "Haversines" -> return Haversines
            "Vincenty" -> return Vincenty
            "Andoyer-Lambert" -> return AndoyerLambert
            "Forsythe-Andoyer-Lambert" -> return ForsytheAndoyerLambert
            "FS-Andoyer" -> return FsAndoyer
            _ -> empty

    parseJSON _ = empty

data Projection = UTM
    deriving (Eq, Ord, Show)

instance ToJSON Projection where
    toJSON = const $ String "UTM"

instance FromJSON Projection where
    parseJSON _ = return UTM

data EarthModel a
    = EarthAsSphere {radius :: QRadius a [u| m |]}
    | EarthAsEllipsoid (Ellipsoid a)
    | EarthAsFlat {projection :: Projection}
    deriving (Eq, Ord, Show, Generic)

earthModelCtorTag :: String -> String
earthModelCtorTag s
    | s == "EarthAsSphere" = "sphere"
    | s == "EarthAsEllipsoid" = "ellipsoid"
    | s == "EarthAsFlat" = "flat"
    | otherwise = s

instance ToJSON (EarthModel Double) where
    toJSON = genericToJSON $
        defaultOptions
            { sumEncoding = ObjectWithSingleField
            , constructorTagModifier = earthModelCtorTag
            }

instance FromJSON (EarthModel Double) where
    parseJSON = genericParseJSON $
        defaultOptions
            { sumEncoding = ObjectWithSingleField
            , constructorTagModifier = earthModelCtorTag
            }

