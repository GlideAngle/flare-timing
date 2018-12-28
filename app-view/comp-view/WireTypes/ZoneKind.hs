module WireTypes.ZoneKind
    ( Shape(..)
    , ZoneKind(..)
    , TaskZones(..)
    , Incline(..)
    , Altitude(..)
    , Radius(..)
    , showRadius
    , showShape
    ) where

import Text.Printf (printf)
import Control.Applicative (empty)
import Data.Foldable (asum)
import Data.Aeson (Value(..), FromJSON(..), (.:), withObject)
import qualified Data.Text as T

newtype Incline = Incline Double
    deriving (Eq, Ord, Show)

newtype Altitude = Altitude Double
    deriving (Eq, Ord, Show)

newtype Radius = Radius Double
    deriving (Eq, Ord, Show)

instance FromJSON Incline where
    parseJSON x@(String _) = do
        s <- reverse . T.unpack <$> parseJSON x
        case s of
            'g' :'e' :  'd' : ' ' : xs -> return . Incline . read . reverse $ xs
            _ -> empty
    parseJSON _ = empty

instance FromJSON Altitude where
    parseJSON x@(String _) = do
        s <- reverse . T.unpack <$> parseJSON x
        case s of
            'm' : ' ' : xs -> return . Altitude . read . reverse $ xs
            _ -> empty
    parseJSON _ = empty

instance FromJSON Radius where
    parseJSON x@(String _) = do
        s <- reverse . T.unpack <$> parseJSON x
        case s of
            'm' : ' ' : xs -> return . Radius . read . reverse $ xs
            _ -> empty
    parseJSON _ = empty

showIncline :: Incline -> String
showIncline (Incline r) =
    printf "%.3f °" r

showAltitude :: Altitude -> String
showAltitude (Altitude r) =
    show (truncate r :: Integer) ++ " m"

showRadius :: Radius -> String
showRadius (Radius r)
    | r < 1000 = show (truncate r :: Integer) ++ " m"
    | otherwise = show (truncate (r / 1000) :: Integer) ++ " km"

data Shape
    = Line
    | Circle
    | SemiCircle
    | Cylinder
    | CutCylinder
    | CutCone
    | CutSemiCylinder
    | CutSemiCone Incline Radius Altitude
    deriving (Eq, Ord, Show)

data ZoneKind = ZoneKind { goalShape :: Shape }
    deriving (Eq, Ord, Show)

showShape :: Shape -> String
showShape Line = "line"
showShape Circle = "circle"
showShape SemiCircle = "semicircle"
showShape Cylinder = "cylinder"
showShape CutCylinder = "cut cylinder"
showShape CutCone = "cut cone"
showShape CutSemiCylinder = "cut semi-cylinder"
showShape (CutSemiCone i r a) =
    "semi-cone with an incline of "
    ++ showIncline i
    ++ " and a radius of "
    ++ showRadius r
    ++ " at an altitude of "
    ++ showAltitude a

instance FromJSON ZoneKind where
    parseJSON = withObject "ZoneKind" $ \o ->
        asum
            [ do
                Object _ <- o .: "line"
                return $ ZoneKind { goalShape = Line }
            , do
                Object _ <- o .: "circle"
                return $ ZoneKind { goalShape = Circle }
            , do
                Object _ <- o .: "semicircle"
                return $ ZoneKind { goalShape = SemiCircle }
            , do
                Object _ <- o .: "cylinder"
                return $ ZoneKind { goalShape = Cylinder }
            , do
                Object _ <- o .: "cut-cylinder"
                return $ ZoneKind { goalShape = CutCylinder }
            , do
                Object _ <- o .: "cut-cone"
                return $ ZoneKind { goalShape = CutCone }
            , do
                Object _ <- o .: "cut-semi-cylinder"
                return $ ZoneKind { goalShape = CutSemiCylinder }
            , do
                sc <- o .: "cut-semi-cone"
                i <- sc .: "incline"
                r <- sc .: "radius"
                a <- sc .: "altitude"
                return ZoneKind {goalShape = CutSemiCone i r a}
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
