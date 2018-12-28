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

newtype TimeBonus = TimeBonus Double
    deriving (Eq, Ord, Show)

newtype Incline = Incline Double
    deriving (Eq, Ord, Show)

newtype Altitude = Altitude Double
    deriving (Eq, Ord, Show)

newtype Radius = Radius Double
    deriving (Eq, Ord, Show)

instance FromJSON TimeBonus where
    parseJSON x@(String _) = do
        s <- reverse . T.unpack <$> parseJSON x
        case s of
            'm' : ' ' : '/' : ' ' : 's' : ' ' : xs -> return . TimeBonus . read . reverse $ xs
            _ -> empty
    parseJSON _ = empty

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

showTimeBonus :: TimeBonus -> String
showTimeBonus (TimeBonus r) =
    printf "%.3f s/m" r

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
    | CutCylinder TimeBonus Radius Altitude
    | CutCone Incline Radius Altitude
    | CutSemiCylinder TimeBonus Radius Altitude
    | CutSemiCone Incline Radius Altitude
    deriving (Eq, Ord, Show)

data ZoneKind = ZoneKind Shape
    deriving (Eq, Ord, Show)

showShape :: Shape -> String
showShape Line = "line"
showShape Circle = "circle"
showShape SemiCircle = "semicircle"
showShape Cylinder = "cylinder"
showShape (CutCylinder t r a) =
    "cut cylinder with a time bonus of "
    ++ showTimeBonus t
    ++ " and a radius of "
    ++ showRadius r
    ++ " at an altitude of "
    ++ showAltitude a
showShape (CutCone i r a) =
    "cut cone with an incline of "
    ++ showIncline i
    ++ " and a radius of "
    ++ showRadius r
    ++ " at an altitude of "
    ++ showAltitude a
showShape (CutSemiCylinder t r a) =
    "cut semi-cylinder with a time bonus of "
    ++ showTimeBonus t
    ++ " and a radius of "
    ++ showRadius r
    ++ " at an altitude of "
    ++ showAltitude a
showShape (CutSemiCone i r a) =
    "cut semi-cone with an incline of "
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
                return $ ZoneKind Line

            , do
                Object _ <- o .: "circle"
                return $ ZoneKind Circle

            , do
                Object _ <- o .: "semicircle"
                return $ ZoneKind SemiCircle

            , do
                Object _ <- o .: "cylinder"
                return $ ZoneKind Cylinder

            , do
                cy <- o .: "cut-cylinder"
                t <- cy .: "time-bonus"
                r <- cy .: "radius"
                a <- cy .: "altitude"
                return $ ZoneKind (CutCylinder t r a)

            , do
                cc <- o .: "cut-cone"
                i <- cc .: "incline"
                r <- cc .: "radius"
                a <- cc .: "altitude"
                return $ ZoneKind (CutCone i r a)

            , do
                cs <- o .: "cut-semi-cylinder"
                t <- cs .: "time-bonus"
                r <- cs .: "radius"
                a <- cs .: "altitude"
                return $ ZoneKind (CutSemiCylinder t r a)

            , do
                sc <- o .: "cut-semi-cone"
                i <- sc .: "incline"
                r <- sc .: "radius"
                a <- sc .: "altitude"
                return $ ZoneKind (CutSemiCone i r a)
            ]

data TaskZones
    = TzEssIsGoal ZoneKind
    | TzEssIsNotGoal ZoneKind ZoneKind
    deriving (Eq, Ord, Show)

instance FromJSON TaskZones where
    parseJSON = withObject "Race" $ \o ->
        asum
            [ do
                e :: ZoneKind <- o .: "race-ess"
                g :: ZoneKind <- o .: "goal"
                return $ TzEssIsNotGoal e g

            , do
                g :: ZoneKind <- o .: "race-ess-is-goal"
                return $ TzEssIsGoal g
            ]
