module WireTypes.ZoneKind
    ( Shape(..)
    , ZoneKind(..)
    , TaskZones(..)
    ) where

import Data.Foldable (asum)
import Data.Aeson (Value(..), FromJSON(..), (.:), withObject)

data Shape = Circle | Line
    deriving (Eq, Ord, Show)

data ZoneKind = ZoneKind { goalShape :: Shape }
    deriving (Eq, Ord, Show)

instance FromJSON ZoneKind where
    parseJSON = withObject "ZoneKind" $ \o ->
        asum
            [ do
                Object _ <- o .: "line"
                return $ ZoneKind { goalShape = Line }
            , do
                Object _ <- o .: "circle"
                return $ ZoneKind { goalShape = Circle }
            ]

data TaskZones
    = TzEssIsGoal ZoneKind
    | TzEssIsNotGoal ZoneKind
    deriving (Eq, Ord, Show)

instance FromJSON TaskZones where
    parseJSON = withObject "Race" $ \o ->
        asum
            [ do
                g :: ZoneKind <- o .: "goal"
                return $ TzEssIsNotGoal g

            , do
                g :: ZoneKind <- o .: "race-ess-is-goal"
                return $ TzEssIsGoal g
            ]
