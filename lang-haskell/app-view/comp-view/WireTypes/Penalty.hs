{-# LANGUAGE DuplicateRecordFields #-}

module WireTypes.Penalty
    ( PointPenalty(..)
    , Add, Mul, Reset
    , PenaltySeqs(..)
    , effectiveMul, effectiveAdd, effectiveReset
    , pprEffectiveMul, pprEffectiveAdd, pprNthAdd, pprEffectiveReset
    ) where

import Text.Printf (printf)
import Data.Maybe (listToMaybe, isJust)
import Data.List (sort)
import GHC.Generics (Generic)
import Data.Aeson ((.:), FromJSON(..), withObject)
import qualified Data.Text as T (Text, pack)

data Add
data Mul
data Reset

data PointPenalty a where
    PenaltyFraction :: Double -> PointPenalty Mul
    PenaltyPoints :: Double -> PointPenalty Add
    PenaltyReset :: Maybe Int -> PointPenalty Reset

instance Eq (PointPenalty a) where
    (==) (PenaltyFraction x) (PenaltyFraction y) = x == y
    (==) (PenaltyPoints x) (PenaltyPoints y) = x == y
    (==) (PenaltyReset x) (PenaltyReset y) = x == y

instance Ord (PointPenalty a) where
    compare (PenaltyFraction x) (PenaltyFraction y) = x `compare` y
    compare (PenaltyPoints x) (PenaltyPoints y) = x `compare` y
    compare (PenaltyReset x) (PenaltyReset y) = x `compare` y

-- NOTE: For pretty printing penalized [-] or rewarded [+].
pprPointPenalty :: Int -> PointPenalty a -> String
pprPointPenalty _ (PenaltyFraction 1) = "* id"
pprPointPenalty _ (PenaltyPoints 0) = "- id"
pprPointPenalty _ (PenaltyReset Nothing) = "= id"

-- | Convert from scaling to fraction of points.
-- The inverse of the function f(x) = 1 - x is f(x) = 1 - x.
pprPointPenalty dp (PenaltyFraction x) = let y = 1 - x in
    if y < 0
        then printf "+ %.*f*" dp y
        else printf "- %.*f*" dp y

pprPointPenalty dp (PenaltyPoints x) =
    if x < 0
        -- NOTE: The + printf flag will show the sign symbol. I want the symbol
        -- then a space before the number. The sign here is showing whether it
        -- is a penalty or a bonus.
        then printf "- %.*f" dp (abs x)
        else printf "+ %.*f" dp x

pprPointPenalty _ (PenaltyReset (Just x)) = printf "= %d" x

instance Show (PointPenalty a) where
    show (PenaltyFraction 1) = "(* id)"
    show (PenaltyPoints 0) = "(+ id)"
    show (PenaltyReset Nothing) = "(= id)"

    show (PenaltyFraction x) = printf "(* %.*f)" x
    show (PenaltyPoints x) =
        if x < 0
            then printf "(- %.*f)" (abs x)
            else printf "(+ %.*f)" x
    show (PenaltyReset (Just x)) = printf "(= %d)" x

instance Num (PointPenalty Mul) where
    (+) _ _ = error "(+) is not defined for PointPenalty Mul."
    (*) (PenaltyFraction a) (PenaltyFraction b) = PenaltyFraction $ a * b
    negate (PenaltyFraction x) = PenaltyFraction $ negate x
    abs (PenaltyFraction x) = PenaltyFraction $ abs x
    signum (PenaltyFraction x) = PenaltyFraction $ signum x
    fromInteger x = PenaltyFraction $ fromInteger x

instance Num (PointPenalty Add) where
    (+) (PenaltyPoints a) (PenaltyPoints b) = PenaltyPoints $ a + b
    (*) _ _ = error "(*) is not defined for PointPenalty Add."
    negate (PenaltyPoints x) = PenaltyPoints $ negate x
    abs (PenaltyPoints x) = PenaltyPoints $ abs x
    signum (PenaltyPoints x) = PenaltyPoints $ signum x
    fromInteger x = PenaltyPoints $ fromInteger x

instance Num (PointPenalty Reset) where
    (+) x@(PenaltyReset (Just _)) (PenaltyReset Nothing) = x
    (+) (PenaltyReset Nothing) x = x
    (+) _ _ = partialNum

    (*) x@(PenaltyReset (Just _)) (PenaltyReset Nothing) = x
    (*) (PenaltyReset Nothing) x = x
    (*) _ _ = partialNum

    negate (PenaltyReset (Just x)) = PenaltyReset . Just $ negate x
    negate x = x

    abs (PenaltyReset (Just x)) = PenaltyReset . Just $ abs x
    abs x = x

    signum _ = PenaltyReset Nothing

    fromInteger x =
        PenaltyReset $
        if x < 0 then Nothing else Just $ fromIntegral x

partialNum :: a
partialNum = error "(+) and (*) are partial for PointPenalty Reset."

instance FromJSON (PointPenalty Mul) where
    parseJSON = withObject "PenaltyPoints" $ \o ->
        PenaltyFraction <$> o .: "penalty-fraction"

instance FromJSON (PointPenalty Add) where
    parseJSON = withObject "PenaltyPoints" $ \o ->
        PenaltyPoints <$> o .: "penalty-points"

instance FromJSON (PointPenalty Reset) where
    parseJSON = withObject "PenaltyPoints" $ \o ->
        PenaltyReset <$> o .: "penalty-reset"

data PenaltySeqs =
    PenaltySeqs
        { muls :: [PointPenalty Mul]
        , adds :: [PointPenalty Add]
        , resets :: [PointPenalty Reset]
        }
    deriving (Eq, Ord, Show, Generic, FromJSON)

effectiveMul :: [PointPenalty Mul] -> PointPenalty Mul
effectiveMul = product

effectiveAdd :: [PointPenalty Add] -> PointPenalty Add
effectiveAdd = sum

effectiveReset :: [PointPenalty Reset] -> PointPenalty Reset
effectiveReset =
    maybe (PenaltyReset Nothing) id
    . listToMaybe
    . take 1
    . sort
    . filter isJustReset

isJustReset :: PointPenalty Reset -> Bool
isJustReset (PenaltyReset x) = isJust x

pprEffectiveMul :: Int -> [PointPenalty Mul] -> T.Text
pprEffectiveMul dp = T.pack . pprPointPenalty dp . effectiveMul

pprNthAdd :: Int -> Int -> [PointPenalty Add] -> T.Text
pprNthAdd dp n xs =
    case take 1 $ drop n xs of
        x : _ -> T.pack $ pprPointPenalty dp x
        _ -> ""

pprEffectiveAdd :: Int -> [PointPenalty Add] -> T.Text
pprEffectiveAdd dp = T.pack . pprPointPenalty dp . effectiveAdd

pprEffectiveReset :: Int -> [PointPenalty Reset] -> T.Text
pprEffectiveReset dp = T.pack . pprPointPenalty dp . effectiveReset
