{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Flight.Gap.Penalty
    ( PointPenalty
    , TooEarlyPoints(..)
    , LaunchToStartPoints(..)
    , PosInt, GE
    , Add, Mul, Reset
    , PointsReduced(..)
    , PenaltySeq(..)
    , PenaltySeqs(..)
    , Hide(..)
    , applyPenalties
    , applyMul, applyAdd, applyReset
    , effectiveMul, effectiveAdd, effectiveReset
    , idSeq, nullSeqs, toSeqs
    , idMul, idAdd, idReset
    , mulSeq, addSeq, resetSeq
    , seqOnlyMuls, seqOnlyAdds, seqOnlyResets
    , mkMul, mkAdd, mkReset
    , exMul, exAdd, exReset
    , identityOfMul, identityOfAdd, identityOfReset
    ) where

import qualified Data.CReal as ExactReal
import Test.QuickCheck (Arbitrary(..), Gen, NonNegative(..))
import Data.Semigroup
import Data.Typeable
import GHC.TypeLits (Nat, KnownNat, natVal)
import Data.Proxy (Proxy(..))
import Text.Printf (printf)
import Data.Refined
    ( Prop, PropProjection, Refined
    , checkProp, assumeProp, refined, unrefined
    )
import Data.Foldable (asum)
import Data.Maybe (listToMaybe, isJust)
import Data.List (sort)
import GHC.Generics (Generic)
import Data.Aeson
    ( ToJSON(..), FromJSON(..), (.:), (.=)
    , object, withObject
    )

import Flight.Gap.Points.Task (TaskPoints(..))
import Data.Ratio.Rounding (sdRound)

type CReal = ExactReal.CReal 12

roundCReal :: CReal -> Double
roundCReal = fromRational . sdRound 3 . toRational

data GE (n :: Nat) deriving Generic

instance (Integral a, KnownNat n, Show a, Generic (GE n)) => Prop a (GE n) where
  type PropProjection a (GE n) = a
  checkProp Proxy n =
    let expected = fromIntegral (natVal (Proxy :: Proxy n)) in
    if n >= expected
        then Right n
        else Left $ printf "Not %s >= %s" (show n) (show expected)

type PosInt = Refined '[GE 0] Int

instance ToJSON (Refined '[GE 0] Int) where
    toJSON = toJSON . unrefined
instance FromJSON (Refined '[GE 0] Int) where
    parseJSON o = assumeProp @(GE 0) . refined <$> parseJSON o

-- NOTE: Reset points are the final points awarded and so can be ints.
data LaunchToStartPoints = LaunchToStartPoints PosInt
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data TooEarlyPoints = TooEarlyPoints PosInt
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data Add
data Mul
data Reset

data PointPenalty a where

    -- | If positive then remove this fraction of points and if negative add this
    -- fraction of points. Unlike FS that uses 0 as a sentinel value meaning no
    -- penalty in all penalty applications, for a fraction we're using the
    -- identity of multiplication here for that purpose.
    PenaltyFraction :: CReal -> PointPenalty Mul

    -- | If positive then remove this number of points and if negative add this
    -- number of points.
    PenaltyPoints :: CReal -> PointPenalty Add

    -- | Reset points down to this natural number.
    PenaltyReset :: Maybe PosInt -> PointPenalty Reset

instance Eq (PointPenalty a) where
    (==) (PenaltyFraction x) (PenaltyFraction y) = x == y
    (==) (PenaltyPoints x) (PenaltyPoints y) = x == y
    (==) (PenaltyReset x) (PenaltyReset y) = x == y

instance Ord (PointPenalty a) where
    compare (PenaltyFraction x) (PenaltyFraction y) = x `compare` y
    compare (PenaltyPoints x) (PenaltyPoints y) = x `compare` y
    compare (PenaltyReset x) (PenaltyReset y) = x `compare` y

instance Show (PointPenalty a) where
    show (PenaltyFraction 1) = "(* id)"
    show (PenaltyPoints 0) = "(+ id)"
    show (PenaltyReset Nothing) = "(= id)"

    show (PenaltyFraction x) = printf "(* %f)" (roundCReal x)
    show (PenaltyPoints x) =
        if x < 0
           then printf "(- %f)" (roundCReal $ abs x)
           else printf "(+ %f)" (roundCReal x)
    show (PenaltyReset (Just x)) = printf "(= %d)" $ unrefined x

instance Semigroup (PointPenalty Mul) where
    (<>) (PenaltyFraction a) (PenaltyFraction b) = PenaltyFraction $ a * b
instance Semigroup (PointPenalty Add) where
    (<>) (PenaltyPoints a) (PenaltyPoints b) = PenaltyPoints $ a + b
instance Semigroup (PointPenalty Reset) where
    (<>) a (PenaltyReset Nothing) = a
    (<>) (PenaltyReset Nothing) b = b
    (<>) (PenaltyReset (Just a)) (PenaltyReset (Just b)) =
        PenaltyReset . Just $ min a b

instance Arbitrary CReal where
    arbitrary = realToFrac <$> (arbitrary :: Gen Double)

instance Arbitrary (PointPenalty Mul) where
    arbitrary = PenaltyFraction . realToFrac <$> (arbitrary :: Gen Double)
instance Arbitrary (PointPenalty Add) where
    arbitrary = PenaltyPoints . realToFrac <$> (arbitrary :: Gen Double)
instance Arbitrary (PointPenalty Reset) where
    arbitrary = do
        x <- arbitrary :: Gen (Maybe (NonNegative Int))
        return . PenaltyReset $ (\(NonNegative y) -> assumeProp $ refined y) <$> x

-- |
-- >>> lawsCheck (monoidLaws (Proxy :: Proxy (PointPenalty Mul)))
-- Monoid: Associative +++ OK, passed 100 tests.
-- Monoid: Left Identity +++ OK, passed 100 tests.
-- Monoid: Right Identity +++ OK, passed 100 tests.
-- Monoid: Concatenation +++ OK, passed 100 tests.
instance Semigroup (PointPenalty Mul) => Monoid (PointPenalty Mul) where
    mempty = identityOfMul
    mappend = (<>)
instance Semigroup (PointPenalty Add) => Monoid (PointPenalty Add) where
    mempty = identityOfAdd
    mappend = (<>)
instance Semigroup (PointPenalty Reset) => Monoid (PointPenalty Reset) where
    mempty = identityOfReset
    mappend = (<>)

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

    negate (PenaltyReset (Just x)) = PenaltyReset . Just . assumeProp . refined . negate $ unrefined x
    negate x = x

    abs (PenaltyReset (Just x)) = PenaltyReset . Just . assumeProp . refined . abs $ unrefined x
    abs x = x

    signum _ = PenaltyReset Nothing

    fromInteger x =
        PenaltyReset $
        if x < 0 then Nothing else Just . assumeProp . refined $ fromIntegral x

partialNum :: a
partialNum = error "(+) and (*) are partial for PointPenalty Reset."

-- | @Mul@ and @Add@ penalties have identity values, @Reset@ ones do not. They
-- are applied in that order.
data PenaltySeq =
    PenaltySeq
        { mul :: PointPenalty Mul
        , add :: PointPenalty Add
        , reset :: PointPenalty Reset
        }
    deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

data PenaltySeqs =
    PenaltySeqs
        { muls :: [PointPenalty Mul]
        , adds :: [PointPenalty Add]
        , resets :: [PointPenalty Reset]
        }
    deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

data PointsReduced =
    PointsReduced
        { subtotal :: TaskPoints
        , mulApplied :: TaskPoints
        , addApplied :: TaskPoints
        , resetApplied :: TaskPoints
        , total :: TaskPoints
        , effp :: PenaltySeq
        , effj :: PenaltySeq
        }
    deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

identityOfMul :: PointPenalty Mul
identityOfMul = PenaltyFraction 1

identityOfAdd :: PointPenalty Add
identityOfAdd = PenaltyPoints 0

identityOfReset :: PointPenalty Reset
identityOfReset = PenaltyReset Nothing

idMul :: PointPenalty Mul -> Bool
idMul = (==) identityOfMul

idAdd :: PointPenalty Add -> Bool
idAdd = (==) identityOfAdd

idReset :: PointPenalty Reset -> Bool
idReset = (==) identityOfReset

-- | The units of each kind of penalty that will not change the points when
-- applied.
--
-- >>> total $ applyPenalties (muls nullSeqs) (adds nullSeqs) (resets nullSeqs) (TaskPoints 0)
-- 0.000
--
-- >>> total $ applyPenalties (muls nullSeqs) (adds nullSeqs) (resets nullSeqs) (TaskPoints 1)
-- 1.000
--
-- >>> total $ applyPenalties (muls nullSeqs) (adds nullSeqs) (resets nullSeqs) (TaskPoints (-1))
-- 0.000
--
-- >>> total $ applyPenalties (muls nullSeqs) (adds nullSeqs) (resets nullSeqs) (TaskPoints 0.5)
-- 0.500
--
-- >>> total $ applyPenalties (muls nullSeqs) (adds nullSeqs) (resets nullSeqs) (TaskPoints 0.8584411845461164)
-- 0.858
--
-- prop> \x -> x >= 0 ==> let pts = TaskPoints x in pts == total (applyPenalties (muls nullSeqs) (adds nullSeqs) (resets nullSeqs) pts)
--
-- prop> \x -> x < 0 ==> let pts = TaskPoints x in (TaskPoints 0) == total (applyPenalties (muls nullSeqs) (adds nullSeqs) (resets nullSeqs) pts)
idSeq :: PenaltySeq
idSeq =
    PenaltySeq
        { mul = PenaltyFraction 1
        , add = PenaltyPoints 0
        , reset = PenaltyReset Nothing
        }

mkMul :: Double -> PointPenalty Mul
mkMul 0 = PenaltyFraction 0
mkMul 1 = PenaltyFraction 1
mkMul x = PenaltyFraction $ realToFrac x

mkAdd :: Double -> PointPenalty Add
mkAdd 0 = PenaltyPoints 0
mkAdd x = PenaltyPoints $ realToFrac x

mkReset :: Maybe Int -> PointPenalty Reset
mkReset Nothing = PenaltyReset Nothing
mkReset (Just x) =
    if | x < 0 -> 0
       | x > 1000 -> error "Points cannot be reset to greater than 1000"
       | otherwise -> PenaltyReset . Just . assumeProp $ refined x

exMul :: PointPenalty Mul -> Double
exMul (PenaltyFraction 0) = 0
exMul (PenaltyFraction 1) = 1
exMul (PenaltyFraction x) = realToFrac x

exAdd :: PointPenalty Add -> Double
exAdd (PenaltyPoints 0) = 0
exAdd (PenaltyPoints x) = realToFrac x

exReset :: PointPenalty Reset -> Maybe Int
exReset (PenaltyReset x) = unrefined <$> x

-- | Construct a seq with only the given @Mul@ penalty.
--
-- prop> \x -> x /= 1 ==> seqOnlyMuls (mulSeq x) == Just (mkMul x)
mulSeq :: Double -> PenaltySeq
mulSeq x = idSeq{mul = mkMul x}

-- | Construct a seq with only the given @Add@ penalty.
--
-- prop> \x -> x /= 0 ==> seqOnlyAdds (addSeq x) == Just (mkAdd x)
addSeq :: Double -> PenaltySeq
addSeq x = idSeq{add = mkAdd x}

-- | Construct a seq with only the given @Reset@ penalty.
resetSeq :: Maybe Int -> PenaltySeq
resetSeq x = idSeq{reset = mkReset x}

-- | Construct empty sequences.
--
-- >>> idMul (effectiveMul (muls nullSeqs))
-- True
--
-- >>> idAdd (effectiveAdd (adds nullSeqs))
-- True
--
-- >>> idReset (effectiveReset (resets nullSeqs))
-- True
nullSeqs :: PenaltySeqs
nullSeqs = PenaltySeqs{muls = [], adds = [], resets = []}

-- | Each operation is put in a singleton list of its kind of operation unless
-- it is the unit of that operation in which case the list for that operation
-- is empty.
--
-- >>> toSeqs idSeq == nullSeqs
-- True
toSeqs :: PenaltySeq -> PenaltySeqs
toSeqs PenaltySeq{mul, add, reset} =
    PenaltySeqs
        { muls = if idMul mul then [] else [mul]
        , adds = if idAdd add then [] else [add]
        , resets = if idReset reset then [] else [reset]
        }

-- | If only non-unit value is the @Mul@ then extract that.
--
-- >>> seqOnlyMuls idSeq
-- Nothing
--
-- >>> seqOnlyMuls (mulSeq 1)
-- Nothing
--
-- >>> seqOnlyMuls (mulSeq 0.3)
-- Just (* 0.3)
--
-- >>> seqOnlyMuls (mulSeq 0.9)
-- Just (* 0.9)
--
-- >>> seqOnlyMuls (addSeq 0)
-- Nothing
--
-- >>> seqOnlyMuls (addSeq 1)
-- Nothing
--
-- >>> seqOnlyMuls (resetSeq Nothing)
-- Nothing
--
-- >>> seqOnlyMuls (resetSeq (Just 0))
-- Nothing
seqOnlyMuls :: PenaltySeq -> Maybe (PointPenalty Mul)
seqOnlyMuls x
    | idAdd (add x) && idReset (reset x) =
        case mul x of
           (idMul -> True) -> Nothing
           y -> Just y
    | otherwise = Nothing

-- | If only non-unit value is the @Add@ then extract that.
--
-- >>> seqOnlyAdds idSeq
-- Nothing
--
-- >>> seqOnlyAdds (mulSeq 1)
-- Nothing
--
-- >>> seqOnlyAdds (mulSeq 0.9)
-- Nothing
--
-- >>> seqOnlyAdds (addSeq 0)
-- Nothing
--
-- >>> seqOnlyAdds (addSeq 1)
-- Just (+ 1.0)
--
-- >>> seqOnlyAdds (resetSeq Nothing)
-- Nothing
--
-- >>> seqOnlyAdds (resetSeq (Just 0))
-- Nothing
seqOnlyAdds :: PenaltySeq -> Maybe (PointPenalty Add)
seqOnlyAdds x
    | idMul (mul x) && idReset (reset x) =
        case add x of
           (idAdd -> True) -> Nothing
           y -> Just y
    | otherwise = Nothing

-- | If only non-unit value is the @Reset@ then extract that.
--
-- >>> seqOnlyResets idSeq
-- Nothing
--
-- >>> seqOnlyResets (mulSeq 1)
-- Nothing
--
-- >>> seqOnlyResets (mulSeq 0.9)
-- Nothing
--
-- >>> seqOnlyResets (addSeq 0)
-- Nothing
--
-- >>> seqOnlyResets (addSeq 1)
-- Nothing
--
-- >>> seqOnlyResets (resetSeq Nothing)
-- Nothing
--
-- >>> seqOnlyResets (resetSeq (Just 0))
-- Just (= 0)
seqOnlyResets :: PenaltySeq -> Maybe (PointPenalty Reset)
seqOnlyResets x
    | idMul (mul x) && idAdd (add x) =
        case reset x of
           y@(idReset -> False) -> Just y
           _ -> Nothing
    | otherwise = Nothing

-- | Applies the penalties, fractionals then absolutes and finally the resets.
applyPenalties
    :: [PointPenalty Mul]
    -> [PointPenalty Add]
    -> [PointPenalty Reset]
    -> TaskPoints
    -> PointsReduced
applyPenalties fracs points resets p =
    let eMul = effectiveMul fracs
        eAdd = effectiveAdd points
        eReset = effectiveReset resets

        withMul = applyPenalty p eMul
        withAdd = applyPenalty withMul eAdd
        withReset = applyPenalty withAdd eReset
    in
        PointsReduced
            { subtotal = p
            , mulApplied = p - withMul
            , addApplied = withMul - withAdd
            , resetApplied = withAdd - withReset
            , total = withReset
            , effp = PenaltySeq eMul eAdd eReset
            , effj = idSeq
            }

-- | Compares a @Mul@ operation against the identity of multiplication.
--
-- >>> cmpMul identityOfMul
-- Just (EQ,1.0)
--
-- prop> \x -> let y = realToFrac x in (realToFrac . snd <$> cmpMul (mkMul y)) == Just y
-- prop> \x -> let y = realToFrac x in (realToFrac . snd <$> cmpMul (mkAdd y)) == Nothing
-- prop> \x -> (snd <$> cmpMul (mkReset y)) == Nothing
cmpMul :: PointPenalty a -> Maybe (Ordering, Double)
cmpMul (PenaltyFraction 1) = Just (EQ, 1)
cmpMul (PenaltyFraction x) = let y = realToFrac x in Just (y `compare` 1, y)
cmpMul _ = Nothing

-- | Compares an @Add@ operation against the identity of addition.
--
-- >>> cmpAdd identityOfAdd
-- Just (EQ,0.0)
--
-- prop> \x -> let y = realToFrac x in (realToFrac . snd <$> cmpAdd (mkMul y)) == Nothing
-- prop> \x -> let y = realToFrac x in (realToFrac . snd <$> cmpAdd (mkAdd y)) == Just y
-- prop> \x -> (snd <$> cmpAdd (mkReset y)) == Nothing
cmpAdd :: PointPenalty a -> Maybe (Ordering, Double)
cmpAdd (PenaltyPoints 0) = Just (EQ, 0)
cmpAdd (PenaltyPoints x) = let y = realToFrac x in Just (y `compare` 0, y)
cmpAdd _ = Nothing

-- | Applies a penalty and ensures the resulting points are not less than zero.
--
-- >>> applyPenalty 2 identityOfMul
-- 2.000
--
-- >>> applyPenalty 2 identityOfAdd
-- 2.000
--
-- >>> applyPenalty 2 identityOfReset
-- 2.000
--
-- >>> applyPenalty 2 (mkMul (-1))
-- 0.000
--
-- >>> applyPenalty 2 (mkAdd (-3))
-- 0.000
--
-- >>> applyPenalty 2 (mkReset $ Just (-1))
-- 0.000
--
-- >>> applyPenalty 2 (mkMul 0)
-- 0.000
--
-- >>> applyPenalty 2 (mkAdd (-2))
-- 0.000
--
-- >>> applyPenalty 2 (mkReset $ Just 0)
-- 0.000
--
-- >>> applyPenalty 2 (mkMul 1.5)
-- 3.000
--
-- >>> applyPenalty 2 (mkAdd 1)
-- 3.000
--
-- >>> applyPenalty 2 (mkReset $ Just 3)
-- 2.000
applyPenalty :: TaskPoints -> PointPenalty a -> TaskPoints
applyPenalty (TaskPoints p) pp = TaskPoints p'
    where
        notNeg = max 0 p

        p'

            -- NOTE: When positive @Add@ penalties are bonuses. A true penalty is
            -- subtraction, adding a negative number. A bonus adds a positive
            -- number.
            | Just (_, n) <- cmpAdd pp = max 0 $ p + n

            -- NOTE: It doesn't matter the magnitude or the sign of the scaling
            -- as we're going to multiply by it anyway.
            | Just (_, n) <- cmpMul pp = max 0 $ p * n

            | PenaltyReset (Just n) <- pp =
                -- NOTE: Resets can only be used as penalties and not bonuses so
                -- we can assume the value is not negative.
                min p (fromIntegral $ unrefined n)

            | Just (EQ, _) <- cmpMul pp = notNeg
            | Just (EQ, _) <- cmpAdd pp = notNeg
            | PenaltyReset Nothing <- pp = notNeg
            | otherwise = notNeg

instance ToJSON CReal where
    toJSON 0 = toJSON (0 :: Double)
    toJSON x = toJSON ((realToFrac x) :: Double)

instance FromJSON CReal where
    parseJSON o = do
        x :: Double <- parseJSON o
        return $ realToFrac x

-- SEE: https://www.reddit.com/r/haskell/comments/5acj3g/derive_fromjson_for_gadts
data Hide f = forall a. Hide (f a)
instance FromJSON (Hide PointPenalty) where
    parseJSON = withObject "PenaltyPoints" $ \o ->
        asum
            [ Hide . PenaltyPoints <$> o .: "penalty-points"
            , Hide . PenaltyFraction <$> o .: "penalty-fraction"
            , Hide . PenaltyReset <$> o .: "penalty-reset"
            ]

instance FromJSON (PointPenalty Mul) where
    parseJSON = withObject "PenaltyPoints" $ \o ->
        PenaltyFraction <$> o .: "penalty-fraction"

instance FromJSON (PointPenalty Add) where
    parseJSON = withObject "PenaltyPoints" $ \o ->
        PenaltyPoints <$> o .: "penalty-points"

instance FromJSON (PointPenalty Reset) where
    parseJSON = withObject "PenaltyPoints" $ \o ->
        PenaltyReset <$> o .: "penalty-reset"

instance ToJSON (PointPenalty a) where
    toJSON (PenaltyPoints x) = object [ "penalty-points" .= toJSON x ]
    toJSON (PenaltyFraction x) = object [ "penalty-fraction" .= toJSON x ]
    toJSON (PenaltyReset x) = object [ "penalty-reset" .= toJSON x ]

instance ToJSON (Hide PointPenalty) where
    toJSON (Hide x) = toJSON x

instance Eq (Hide PointPenalty) where
    (==) (Hide x@PenaltyFraction{}) (Hide y@PenaltyFraction{}) = x == y
    (==) (Hide x@PenaltyPoints{}) (Hide y@PenaltyPoints{}) = x == y
    (==) (Hide x@PenaltyReset{}) (Hide y@PenaltyReset{}) = x == y
    (==) _ _ = False

instance Ord (Hide PointPenalty) where
    compare (Hide x@PenaltyFraction{}) (Hide y@PenaltyFraction{}) = x `compare` y
    compare (Hide x@PenaltyPoints{}) (Hide y@PenaltyPoints{}) = x `compare` y
    compare (Hide x@PenaltyReset{}) (Hide y@PenaltyReset{}) = x `compare` y
    compare x y = error $ printf "Not comparable %s and %s" (show $ typeOf x) (show $ typeOf y)

instance Show (Hide PointPenalty) where
    show (Hide x) = show x

-- | The effective fraction is the sum of the list.
--
-- >>> effectiveMul [] == identityOfMul
-- True
--
-- >>> effectiveMul []
-- (* id)
--
-- >>> effectiveMul [identityOfMul]
-- (* id)
--
-- >>> effectiveMul (replicate 3 identityOfMul)
-- (* id)
--
-- prop> \x -> effectiveMul [x] == x
-- prop> \x -> effectiveMul [x, identityOfMul] == x
-- prop> \x -> effectiveMul [identityOfMul, x] == x
--
-- >>> effectiveMul [mkMul 2, mkMul 3]
-- (* 6.0)
--
-- >>> effectiveMul [mkMul 2, mkMul 0.5]
-- (* id)
effectiveMul :: [PointPenalty Mul] -> PointPenalty Mul
effectiveMul = product

-- | The effective point is the sum of the list.
--
-- >>> effectiveAdd [] == identityOfAdd
-- True
--
-- >>> effectiveAdd []
-- (+ id)
--
-- >>> effectiveAdd [identityOfAdd]
-- (+ id)
--
-- >>> effectiveAdd (replicate 3 identityOfAdd)
-- (+ id)
--
-- prop> \x -> effectiveAdd [x] == x
-- prop> \x -> effectiveAdd [x, identityOfAdd] == x
-- prop> \x -> effectiveAdd [identityOfAdd, x] == x
--
-- >>> effectiveAdd [mkAdd 1, mkAdd 2]
-- (+ 3.0)
--
-- >>> effectiveAdd [mkAdd 1, mkAdd (-1)]
-- (+ id)
effectiveAdd :: [PointPenalty Add] -> PointPenalty Add
effectiveAdd = sum

-- | The effective reset is the minimum of the list.
--
-- >>> effectiveReset [] == identityOfReset
-- True
--
-- >>> effectiveReset []
-- (= id)
--
-- >>> effectiveReset [identityOfReset]
-- (= id)
--
-- >>> effectiveReset (replicate 3 identityOfReset)
-- (= id)
--
-- prop> \x -> effectiveReset [x] == x
-- prop> \x -> effectiveReset [x, identityOfReset] == x
-- prop> \x -> effectiveReset [identityOfReset, x] == x
--
-- >>> effectiveReset [mkReset $ Just 1, mkReset $ Just 2]
-- (= 1)
--
-- >>> effectiveReset [mkReset $ Just 2, mkReset $ Just 1]
-- (= 1)
effectiveReset :: [PointPenalty Reset] -> PointPenalty Reset
effectiveReset =
    maybe (PenaltyReset Nothing) id
    . listToMaybe
    . take 1
    . sort
    . filter isJustReset

isJustReset :: PointPenalty Reset -> Bool
isJustReset (PenaltyReset x) = isJust x

-- | Applies only fractional penalties.
applyMul :: [PointPenalty Mul] -> TaskPoints -> TaskPoints
applyMul fracs p =
    applyPenalty p (effectiveMul fracs)

-- | Applies only point penalties.
applyAdd :: [PointPenalty Add] -> TaskPoints -> TaskPoints
applyAdd points p =
    applyPenalty p (effectiveAdd points)

-- | Applies only reset penalties.
applyReset :: [PointPenalty Reset] -> TaskPoints -> TaskPoints
applyReset resets p =
    applyPenalty p (effectiveReset resets)

-- $setup
-- >>> import Test.QuickCheck.Classes
-- >>> import Data.Proxy
