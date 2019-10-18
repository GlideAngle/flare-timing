module Flight.EastNorth (EastingNorthing(..), UtmZone(..)) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))

data EastingNorthing =
    EastingNorthing { easting :: Double
                    , northing :: Double
                    }
                    deriving (Eq, Ord, Show, Generic)

instance ToJSON EastingNorthing
instance FromJSON EastingNorthing

data UtmZone =
    UtmZone { latZone :: Char
            , lngZone :: Int
            }
            deriving (Eq, Ord, Show, Generic)

instance ToJSON UtmZone
instance FromJSON UtmZone
