module WireTypes.ZoneKind
    ( Shape(..)
    , ZoneKind(..)
    , TaskZones(..)
    , Incline(..)
    , Alt(..)
    , Radius(..)
    , RawLat(..)
    , RawLng(..)
    , showLat
    , showLng
    , showRadius
    , showShape
    , showAlt
    ) where

import Text.Printf (printf)
import Control.Applicative (empty)
import Data.Foldable (asum)
import Data.Aeson (Value(..), FromJSON(..), (.:), withObject)
import qualified Data.Text as T
import Data.Scientific (Scientific, toRealFloat, fromRationalRepetend)

newtype RawLat = RawLat Rational
    deriving (Eq, Ord)

newtype RawLng = RawLng Rational
    deriving (Eq, Ord)

fromSci :: Scientific -> Rational
fromSci x = toRational (toRealFloat x :: Double)

toSci :: Rational -> Scientific
toSci x =
    case fromRationalRepetend (Just 7) x of
        Left (s, _) -> s
        Right (s, _) -> s

instance FromJSON RawLat where
    parseJSON x@(Number _) = RawLat . fromSci <$> parseJSON x
    parseJSON _ = empty

instance FromJSON RawLng where
    parseJSON x@(Number _) = RawLng . fromSci <$> parseJSON x
    parseJSON _ = empty

showLat :: RawLat -> String
showLat (RawLat x) = (show . toSci $ x) ++ " °"

showLng :: RawLng -> String
showLng (RawLng x) = (show . toSci $ x) ++ " °"

instance Show RawLat where show = showLat
instance Show RawLng where show = showLng

newtype Target = Target (RawLat, RawLng)
    deriving (Eq, Ord, Show)

newtype TimeBonus = TimeBonus Double
    deriving (Eq, Ord, Show)

newtype Incline = Incline Double
    deriving (Eq, Ord, Show)

newtype Alt = Alt Double
    deriving (Eq, Ord, Show)

newtype Radius = Radius Double
    deriving (Eq, Ord, Show)

instance FromJSON Target where
    parseJSON x@(Array _) = do
        xys <- parseJSON x
        case xys of
            [sLat, sLng] ->
                case (reverse sLat, reverse sLng) of
                    ( 'g' : 'e' : 'd' : ' ' : xs, 'g' : 'e' : 'd' : ' ' : ys) ->
                        let lat = RawLat . fromSci . read . reverse $ xs
                            lng = RawLng . fromSci . read . reverse $ ys
                        in return $ Target (lat, lng)

                    _ -> empty

            _ -> empty
    parseJSON _ = empty

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
            'g' : 'e' :  'd' : ' ' : xs -> return . Incline . read . reverse $ xs
            _ -> empty
    parseJSON _ = empty

instance FromJSON Alt where
    parseJSON x@(String _) = do
        s <- reverse . T.unpack <$> parseJSON x
        case s of
            'm' : ' ' : xs -> return . Alt . read . reverse $ xs
            _ -> empty
    parseJSON _ = empty

instance FromJSON Radius where
    parseJSON x@(String _) = do
        s <- reverse . T.unpack <$> parseJSON x
        case s of
            'm' : ' ' : xs -> return . Radius . read . reverse $ xs
            _ -> empty
    parseJSON _ = empty

showTarget :: Target -> String
showTarget (Target (lat, lng)) =
    "(" ++ showLat lat ++ ", " ++ showLng lng ++ ")"

showTimeBonus :: TimeBonus -> String
showTimeBonus (TimeBonus r) =
    printf "%.3f s/m" r

showIncline :: Incline -> String
showIncline (Incline r) =
    printf "%.3f °" r

showAlt :: Alt -> String
showAlt (Alt r) =
    show (truncate r :: Integer) ++ " m"

showRadius :: Radius -> String
showRadius (Radius r) =
    show (truncate r :: Integer) ++ " m"

data Shape
    = Line
    | Circle
    | SemiCircle
    | Cylinder
    | CutCylinder TimeBonus Radius Alt
    | CutCone Incline Radius Alt
    | CutSemiCylinder TimeBonus Radius Alt
    | CutSemiCone Incline Radius Alt
    | Vector Target
    | Star
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
    ++ showAlt a
showShape (CutCone i r a) =
    "cut cone with an incline of "
    ++ showIncline i
    ++ " and a radius of "
    ++ showRadius r
    ++ " at an altitude of "
    ++ showAlt a
showShape (CutSemiCylinder t r a) =
    "cut semi-cylinder with a time bonus of "
    ++ showTimeBonus t
    ++ " and a radius of "
    ++ showRadius r
    ++ " at an altitude of "
    ++ showAlt a
showShape (CutSemiCone i r a) =
    "cut semi-cone with an incline of "
    ++ showIncline i
    ++ " and a radius of "
    ++ showRadius r
    ++ " at an altitude of "
    ++ showAlt a
showShape (Vector t) =
    "vector targeting "
    ++ showTarget t
showShape Star = "star"

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

            , do
                vc <- o .: "vector"
                t <- vc .: "target"
                return $ ZoneKind (Vector t)

            , do
                Object _ <- o .: "star"
                return $ ZoneKind Star
            ]

data TaskZones
    = TzEssIsGoal ZoneKind
    | TzEssIsNotGoal ZoneKind ZoneKind
    | TzOpenDistance ZoneKind
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

            , do
                f :: ZoneKind <- o .: "open-free"
                return $ TzOpenDistance f
            ]
