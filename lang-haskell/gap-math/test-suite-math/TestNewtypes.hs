module TestNewtypes where

-- NOTE: Avoid orphan instance warnings with these newtypes.

import Prelude hiding (max)
import qualified Prelude (max)
import Text.Printf (printf)
import Data.Refined (assumeProp, refined)
import Data.Ratio ((%))
import Data.List (sortBy)
import Test.SmallCheck.Series as SC
import Test.Tasty.QuickCheck as QC
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Test.QuickCheck.Instances ()

import Flight.Ratio (pattern (:%), splitRatio)
import "flight-gap-allot" Flight.Score
import "flight-gap-math" Flight.Score

import Normal (Normal(..))
import Flight.Units()

tooEarly1, tooEarly2 :: TooEarlyPoints
tooEarly1 = TooEarlyPoints . assumeProp $ refined 1
tooEarly2 = TooEarlyPoints . assumeProp $ refined 2

-- | Arrival fraction
newtype AfTest = AfTest (PilotsAtEss, ArrivalPlacing) deriving Show

instance Monad m => SC.Serial m AfTest where
    series =
        cons1 $ \(Normal (rank :% n)) ->
             AfTest (PilotsAtEss n, ArrivalPlacing $ Prelude.max 1 rank)

instance QC.Arbitrary AfTest where
    arbitrary = do
        (rank, n) <- splitRatio <$> arbitrary
        return $ AfTest (PilotsAtEss n, ArrivalPlacing $ Prelude.max 1 rank)

-- | Speed fraction
newtype SfTest =
    SfTest
        ( BestTime (Quantity Double [u| h |])
        , PilotTime (Quantity Double [u| h |])
        )
    deriving Show

instance Monad m => SC.Serial m SfTest where
    series =
        cons1 $ \(Normal (n :% d)) ->
             SfTest
                 (BestTime . MkQuantity . fromRational $ (d % 1)
                 , PilotTime . MkQuantity . fromRational $ (d + n) % 1
                 )

instance QC.Arbitrary SfTest where
    arbitrary = do
        (n, d) <- splitRatio <$> arbitrary
        return . SfTest $
            ( BestTime . MkQuantity . fromRational $ (d % 1)
            , PilotTime . MkQuantity . fromRational $ (d + n) % 1
            )

-- | Linear fraction
newtype LfTest =
    LfTest
        ( FlownMax (Quantity Double [u| km |])
        , PilotDistance (Quantity Double [u| km |])
        )
    deriving Show

instance Monad m => SC.Serial m LfTest where
    series =
        cons1 $ \(Normal ((n :% d) :: Rational))  ->
             LfTest
                 ( FlownMax . MkQuantity . fromIntegral $ d + n
                 , PilotDistance . MkQuantity . fromIntegral $ d
                 )

instance QC.Arbitrary LfTest where
    arbitrary = do
        (Normal (x :: Rational)) <- arbitrary
        let (n, d) = splitRatio x
        return . LfTest $
            ( FlownMax . MkQuantity . fromIntegral $ d + n
            , PilotDistance . MkQuantity . fromIntegral $ d
            )

-- | Difficulty fraction
newtype DfTest =
    DfTest
        ( FlownMax (Quantity Double [u| km |])
        , [PilotDistance (Quantity Double [u| km |])]
        )
    deriving Show

mkDfTest :: [Int] -> DfTest
mkDfTest xs =
    case toRational <$> sortBy (flip compare) (abs <$> xs) of
        [] ->
            DfTest
                ( FlownMax . MkQuantity $ 0
                , []
                )

        ys ->
            DfTest
                ( FlownMax . MkQuantity . fromRational $ maximum ys
                , PilotDistance . MkQuantity . fromRational <$> reverse ys
                )

instance Monad m => SC.Serial m DfTest where
    series = cons1 mkDfTest

instance QC.Arbitrary DfTest where
    arbitrary = do
        xs <- listOf $ choose (1, 1000000)
        return $ mkDfTest xs

-- | Task points, tally and penalties.
data PtTest a =
    PtTest
        { ptSitrep :: SitRep a
        , ptEssNotGoal :: GoalValidatedPoints -> PenaltySeq
        , ptJumps :: PenaltySeq
        , ptOthers :: PenaltySeqs
        , ptPoints :: Points
        }

instance Show (PtTest a) where
    show PtTest{..} =
        printf "(situation = %s, jumps = %s, penalties = %s, points = %s)" (show ptSitrep) (show ptJumps) (show ptOthers) (show ptPoints)

newtype PointParts = PointParts Points deriving Show

instance QC.Arbitrary PointParts where
    arbitrary = do
        (QC.Positive d) <- arbitrary
        (QC.Positive l) <- arbitrary
        (QC.Positive t) <- arbitrary
        (QC.Positive a) <- arbitrary
        return . PointParts $ Points
            { reach = LinearPoints $ d / 2
            , effort = DifficultyPoints $ d / 2
            , distance = DistancePoints d
            , leading = LeadingPoints l
            , time = TimePoints t
            , arrival = ArrivalPoints a
            }

genPtTest :: SitRep a -> Gen (PtTest a)
genPtTest penalty = do
    (PointParts parts) <- arbitrary

    jumps :: PenaltySeq <-
        QC.oneof
            [ addSeq <$> arbitrary
            , mulSeq <$> arbitrary
            , (\x -> resetSeq ((\(QC.Positive y) -> y) <$> x)) <$> arbitrary
            ]

    others <- do
        muls <- arbitrary :: Gen [Double]
        adds <- arbitrary :: Gen [Double]
        resets <- arbitrary :: Gen [Maybe (QC.Positive Int)]
        return $
            PenaltySeqs
                (mkMul <$> muls)
                (mkAdd <$> adds)
                (mkReset . fmap (\(QC.Positive y) -> y) <$> resets)

    return $ PtTest penalty egPenaltyNull jumps others parts

instance QC.Arbitrary (PtTest Hg) where
    arbitrary = do
        penalty <-
            QC.oneof
                [ return NominalHg
                , return NoGoalHg
                , do
                    (QC.Positive x) <- arbitrary
                    return $ JumpedTooEarly (TooEarlyPoints (assumeProp $ refined x))

                , do
                    (QC.Positive spp) <- arbitrary
                    (QC.Positive jtg) <- arbitrary
                    return $
                        Jumped
                            tooEarly1
                            (SecondsPerPoint $ MkQuantity spp)
                            (JumpedTheGun $ MkQuantity jtg)

                , do
                    (QC.Positive spp) <- arbitrary
                    (QC.Positive jtg) <- arbitrary
                    return $
                        JumpedNoGoal
                            tooEarly1
                            (SecondsPerPoint $ MkQuantity spp)
                            (JumpedTheGun $ MkQuantity jtg)
                ]

        genPtTest penalty

instance QC.Arbitrary (PtTest Pg) where
    arbitrary = do
        penalty <-
            QC.oneof
                [ return NominalPg
                , return NoGoalPg
                , do
                    (QC.Positive x) <- arbitrary
                    return $ Early (LaunchToStartPoints (assumeProp $ refined x))
                ]

        genPtTest penalty