{-# LANGUAGE ViewPatterns #-}

module Flight.Gap.Penalty
    ( PointPenalty(..)
    , TooEarlyPoints(..)
    , LaunchToStartPoints(..)
    , PosInt, GE
    , Add, Mul, Reset
    , PointsReduced(..)
    , PenaltySeq(..)
    , PenaltySeqs(..)
    , applyPenalties
    , Hide(..)
    , applyMul, applyAdd, applyReset
    , effectiveMul, effectiveAdd, effectiveReset
    , idSeq, nullSeqs, toSeqs
    , idMul, idAdd, idReset
    , mulSeq, addSeq, resetSeq
    , seqOnlyMuls, seqOnlyAdds, seqOnlyResets
    ) where

import Data.Typeable
import GHC.TypeLits (Nat, KnownNat, natVal)
import Data.Proxy (Proxy(..))
import Text.Printf (printf)
import Data.Refined
    ( Prop, PropProjection, Refined
    , checkProp, assumeProp, refined, unrefined
    )
import Data.Foldable (asum)
import Data.Maybe (listToMaybe)
import Data.List (sort)
import GHC.Generics (Generic)
import Data.Aeson
    ( ToJSON(..), FromJSON(..), (.:), (.=)
    , object, withObject
    )

import Flight.Gap.Points.Task (TaskPoints(..))

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
    -- fraction of points.
    PenaltyFraction :: Double -> PointPenalty Mul

    -- | If positive then remove this number of points and if negative add this
    -- number of points.
    PenaltyPoints :: Double -> PointPenalty Add

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

    show (PenaltyFraction x) = printf "(* %f)" x
    show (PenaltyPoints x) = printf "(+ %f)" x
    show (PenaltyReset (Just x)) = printf "(= %d)" $ unrefined x

instance Num (PointPenalty Mul) where
    (+) _ _ = error "(+) is not defined for PointPenalty Mul."
    (*) (PenaltyFraction a) (PenaltyFraction b) = PenaltyFraction $ a * b
    negate (PenaltyFraction x) = PenaltyFraction $ negate x
    abs (PenaltyFraction x) = PenaltyFraction $ abs x
    signum (PenaltyFraction x) = PenaltyFraction $ signum x
    fromInteger x = PenaltyFraction $ fromInteger x

instance Num (PointPenalty Add) where
    (+) (PenaltyPoints a) (PenaltyPoints b) = PenaltyPoints $ a + b
    (*) _ _ = error "(+) is not defined for PointPenalty Add."
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

idMul :: PointPenalty Mul -> Bool
idMul = \case PenaltyFraction 1 -> True; _ -> False

idAdd :: PointPenalty Add -> Bool
idAdd = \case PenaltyPoints 0 -> True; _ -> False

idReset :: PointPenalty Reset -> Bool
idReset = \case PenaltyReset Nothing -> True; PenaltyReset (Just _) -> False

idSeq :: PenaltySeq
idSeq =
    PenaltySeq
        { mul = PenaltyFraction 0
        , add = PenaltyPoints 0
        , reset = PenaltyReset Nothing
        }

mulSeq :: Double -> PenaltySeq
mulSeq x = idSeq{mul = PenaltyFraction x}

addSeq :: Double -> PenaltySeq
addSeq x = idSeq{add = PenaltyPoints x}

resetSeq :: Maybe PosInt -> PenaltySeq
resetSeq x = idSeq{reset = PenaltyReset x}

nullSeqs :: PenaltySeqs
nullSeqs =
    PenaltySeqs
        { muls = []
        , adds = []
        , resets = []
        }

toSeqs :: PenaltySeq -> PenaltySeqs
toSeqs PenaltySeq{mul, add, reset} =
    PenaltySeqs
        { muls = if idMul mul then [] else [mul]
        , adds = if idAdd add then [] else [add]
        , resets = if idReset reset then [] else [reset]
        }

seqOnlyMuls :: PenaltySeq -> Maybe (PointPenalty Mul)
seqOnlyMuls x
    | idAdd (add x) && idReset (reset x) =
        case mul x of
           (idMul -> True) -> Nothing
           y -> Just y
    | otherwise = Nothing

seqOnlyAdds :: PenaltySeq -> Maybe (PointPenalty Add)
seqOnlyAdds x
    | idMul (mul x) && idReset (reset x) =
        case add x of
           (idAdd -> True) -> Nothing
           y -> Just y
    | otherwise = Nothing

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

zP :: PointPenalty a -> Maybe (Ordering, TaskPoints)
zP (PenaltyPoints n) = Just (n `compare` 0, TaskPoints $ abs n)
zP _ = Nothing

zF :: PointPenalty a -> Maybe (Ordering, TaskPoints)
zF (PenaltyFraction n) = Just (n `compare` 0, TaskPoints $ abs n)
zF _ = Nothing

applyPenalty :: TaskPoints -> PointPenalty a -> TaskPoints
applyPenalty p pp

    | Just (EQ, _) <- zP pp = p
    | Just (GT, n) <- zP pp = max 0 $ p - n
    | Just (LT, n) <- zP pp = p + n

    | Just (EQ, _) <- zF pp = p
    | Just (GT, n) <- zF pp = max 0 $ p * (1.0 - n)
    | Just (LT, n) <- zF pp = p * (1.0 + n)

    | PenaltyReset (Just n) <- pp =
        -- NOTE: Resets can only be used as penalties, not bonuses.
        min p (TaskPoints . fromIntegral $ unrefined n)

    | otherwise = p

isPenaltyPoints, isPenaltyFraction, isPenaltyReset :: PointPenalty a -> Bool
isPenaltyPoints = \case PenaltyPoints{} -> True; _ -> False
isPenaltyFraction = \case PenaltyFraction{} -> True; _ -> False
isPenaltyReset = \case PenaltyReset{} -> True; _ -> False

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
effectiveMul :: [PointPenalty Mul] -> PointPenalty Mul
effectiveMul = product . filter isPenaltyFraction

-- | The effective point is the sum of the list.
effectiveAdd :: [PointPenalty Add] -> PointPenalty Add
effectiveAdd = sum . filter isPenaltyPoints

-- | The effective reset is the minimum of the list.
effectiveReset :: [PointPenalty Reset] -> PointPenalty Reset
effectiveReset =
    maybe (PenaltyReset Nothing) id
    . listToMaybe
    . take 1
    . sort
    . filter isPenaltyReset

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
