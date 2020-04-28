module TestNewtypes where

-- NOTE: Avoid orphan instance warnings with these newtypes.

import Prelude hiding (max)
import qualified Prelude (max)
import Data.Ratio ((%))
import Data.List (sortBy)
import Test.SmallCheck.Series as SC
import Test.Tasty.QuickCheck as QC
import qualified Statistics.Sample as Stats (meanVariance)
import qualified Data.Vector as V (fromList)
import Data.UnitsOfMeasure (u, zero)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Test.QuickCheck.Instances ()

import Flight.Ratio (pattern (:%))
import Flight.Score
    ( Lw(..)
    , Aw(..)
    , GoalRatio(..)
    , NominalGoal(..)
    , NominalDistance(..)
    , DistanceRatio(..)
    , DistanceWeight(..)
    , LeadingWeight(..)
    , ArrivalWeight(..)
    , LaunchValidity(..)
    , TimeValidity(..)
    , DistanceValidity(..)
    , PilotsAtEss(..)
    , ArrivalPlacing(..)
    , BestTime(..)
    , PilotTime(..)
    , FlownMax(..)
    , PilotDistance(..)
    , TaskTime(..)
    , DistanceToEss(..)
    , Leg(..)
    , LcTrack
    , LcSeq(..)
    , LcPoint(..)
    , TaskDeadline(..)
    , LengthOfSs(..)
    , LaunchToStartPoints(..)
    , TooEarlyPoints(..)
    , SecondsPerPoint(..)
    , JumpedTheGun(..)
    , Hg
    , Pg
    , SitRep(..)
    , PointPenalty(..)
    , LinearPoints(..)
    , DifficultyPoints(..)
    , DistancePoints(..)
    , LeadingPoints(..)
    , ArrivalPoints(..)
    , TimePoints(..)
    , Points(..)
    , StopTime(..)
    , ScoreBackTime(..)
    , AnnouncedTime(..)
    , StartGateInterval(..)
    , PilotsAtEss(..)
    , CanScoreStopped(..)
    , TaskStopTime(..)
    , PilotsLanded(..)
    , PilotsFlying(..)
    , LaunchToEss(..)
    , PilotDistance(..)
    , TaskType(..)
    , StartGates(..)
    , GlideRatio(..)
    , AltitudeAboveGoal(..)
    , DistanceToGoal(..)
    , StoppedTrack(..)
    , ReachToggle(..)
    , ReachStats(..)
    , FlownMean(..)
    , FlownStdDev(..)
    )

import Normal (Normal(..), NormalSum(..))
import Flight.Units()

-- | Nominal goal.
newtype NgTest = NgTest NominalGoal deriving Show

instance Monad m => SC.Serial m NgTest where
    series = cons1 $ \(Normal x) -> NgTest (NominalGoal x)

instance QC.Arbitrary NgTest where
    arbitrary = arbitrary >>= \(Normal x) -> return $ NgTest (NominalGoal x)

-- | Nominal distance.
newtype NdTest = NdTest (NominalDistance (Quantity Double [u| km |]))
    deriving Show

instance Monad m => SC.Serial m NdTest where
    series =
        cons1
        $ \(SC.NonNegative x) ->
            NdTest (NominalDistance $ MkQuantity x)

instance QC.Arbitrary NdTest where
    arbitrary =
        arbitrary
        >>= \(QC.NonNegative x) ->
            return $ NdTest (NominalDistance $ MkQuantity x)

-- | Leading weight.
newtype LwTest = LwTest Lw deriving Show

instance Monad m => SC.Serial m LwTest where
    series = LwTest <$> (xs \/ ys \/ zs)
        where
            xs = cons1 $ \(Normal x) -> LwHg (DistanceWeight x)
            ys = cons1 $ \(Normal r) -> LwPgZ (DistanceRatio r)
            zs = cons1 $ \(Normal x) -> LwPg (DistanceWeight x)

instance QC.Arbitrary LwTest where
    arbitrary = LwTest <$> arb
        where
        arb = do
            (Normal r) <- arbitrary
            QC.oneof $ return <$> [ LwHg (DistanceWeight r)
                                  , LwPgZ (DistanceRatio r)
                                  , LwPg (DistanceWeight r)
                                  ]

-- | Arrival weight.
newtype AwTest = AwTest Aw deriving Show

instance Monad m => SC.Serial m AwTest where
    series = AwTest <$> (xs \/ ys \/ zs)
        where
            xs = cons0 AwZero
            ys = cons1 $ \(Normal x) -> AwHgRank (DistanceWeight x)
            zs = cons1 $ \(Normal x) -> AwHgTime (DistanceWeight x)

instance QC.Arbitrary AwTest where
    arbitrary = AwTest <$> arb
        where
            arb = do
                (Normal x) <- arbitrary
                QC.oneof $ return <$> [ AwZero
                                      , AwHgRank (DistanceWeight x)
                                      , AwHgTime (DistanceWeight x)
                                      ]

-- | Goal ratio.
newtype GrTest = GrTest GoalRatio deriving Show

instance Monad m => SC.Serial m GrTest where
    series = cons1 $ \(Normal x) -> GrTest (GoalRatio x)

instance QC.Arbitrary GrTest where
    arbitrary = arbitrary >>= \(Normal x) -> return $ GrTest (GoalRatio x)

-- | Time weight.
newtype TwTest = TwTest (DistanceWeight, LeadingWeight, ArrivalWeight) deriving Show

instance Monad m => SC.Serial m TwTest where
    series =
        cons1 $ \(NormalSum (x, y, z)) ->
             TwTest (DistanceWeight x, LeadingWeight y, ArrivalWeight z)

instance QC.Arbitrary TwTest where
    arbitrary = do
        (NormalSum (x, y, z)) <- arbitrary
        return $ TwTest (DistanceWeight x, LeadingWeight y, ArrivalWeight z)

-- | Task validity
newtype TvTest = TvTest (LaunchValidity, DistanceValidity, TimeValidity) deriving Show

instance Monad m => SC.Serial m TvTest where
    series =
        cons1 $ \(NormalSum (x, y, z)) ->
             TvTest (LaunchValidity x, DistanceValidity y, TimeValidity z)

instance QC.Arbitrary TvTest where
    arbitrary = do
        (NormalSum (x, y, z)) <- arbitrary
        return $ TvTest (LaunchValidity x, DistanceValidity y, TimeValidity z)

-- | Arrival fraction
newtype AfTest = AfTest (PilotsAtEss, ArrivalPlacing) deriving Show

instance Monad m => SC.Serial m AfTest where
    series =
        cons1 $ \(Normal (rank :% n)) ->
             AfTest (PilotsAtEss n, ArrivalPlacing $ Prelude.max 1 rank)

instance QC.Arbitrary AfTest where
    arbitrary = do
        (Normal (rank :% n)) <- arbitrary
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
        (Normal (n :% d)) <- arbitrary
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
        (Normal ((n :% d) :: Rational)) <- arbitrary
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

-- | Leading coefficient, clean task.
newtype LcCleanTest = LcCleanTest (LengthOfSs, LcTrack) deriving Show

mkLcTrack :: [Int] -> LcTrack
mkLcTrack xs =
    case toRational <$> xs of
         [] -> LcSeq []
         ys ->
             LcSeq{seq = pts}
             where
                 ts = TaskTime . MkQuantity <$> [1 .. ]
                 ds = DistanceToEss . MkQuantity <$> ys
                 pts =
                     zipWith
                         (\t d -> LcPoint{leg = RaceLeg 0, mark = t, togo = d})
                         ts
                         ds

mkLcCleanTest :: Rational -> [Int] -> LcCleanTest
mkLcCleanTest len xs =
    LcCleanTest (LengthOfSs $ MkQuantity len, mkLcTrack xs)

instance Monad m => SC.Serial m LcCleanTest where
    series = cons2 (\(SC.Positive len) xs -> mkLcCleanTest len xs)

instance QC.Arbitrary LcCleanTest where
    arbitrary = do
        (QC.Positive len) <- arbitrary
        xs <- listOf $ choose (1, 1000000)
        return $ mkLcCleanTest len xs

-- | Leading coefficient, leading fractions.
newtype LcTest = LcTest (TaskDeadline, LengthOfSs, [LcTrack]) deriving Show

mkLcTest :: Rational -> Rational -> [[Int]] -> LcTest
mkLcTest deadline len xs =
    LcTest (TaskDeadline $ MkQuantity deadline, LengthOfSs $ MkQuantity len, mkLcTrack <$> xs)

instance Monad m => SC.Serial m LcTest where
    series =
        cons3
            (\(SC.Positive deadline) (SC.Positive len) xs ->
                mkLcTest deadline len xs)

instance QC.Arbitrary LcTest where
    arbitrary = do
        (QC.Positive deadline) <- arbitrary
        (QC.Positive len) <- arbitrary
        xs <- listOf $ listOf $ choose (1, 1000000)
        return $ mkLcTest deadline len xs

-- | Task points, tally and penalties.
data PtTest a =
    PtTest
        { ptSitrep :: SitRep a
        , ptJumps :: [PointPenalty]
        , ptOthers :: [PointPenalty]
        , ptPoints :: Points
        }
    deriving Show

instance Monad m => SC.Serial m (PtTest Hg) where
    series = decDepth $ mkPtTest <$> penalty <~> jumps <~> others <~> cons4 mkParts
        where
            mkPtTest :: SitRep Hg -> [PointPenalty] -> [PointPenalty] -> Points -> PtTest Hg
            mkPtTest a b c d = PtTest a b c d

            mkParts
                (SC.Positive d)
                (SC.Positive l)
                (SC.Positive t)
                (SC.Positive a) =

                Points
                    { reach = LinearPoints $ d / 2
                    , effort = DifficultyPoints $ d / 2
                    , distance = DistancePoints d
                    , leading = LeadingPoints l
                    , time = TimePoints t
                    , arrival = ArrivalPoints a
                    }

            jumps =
                cons1 (pure . PenaltyPoints)
                \/ cons1 (pure . PenaltyFraction)
                \/ cons1 (pure . PenaltyReset)

            others =
                cons1 (pure . PenaltyPoints)
                \/ cons1 (pure . PenaltyFraction)
                \/ cons1 (pure . PenaltyReset)


            penalty =
                cons0 NominalHg
                \/ cons0 NoGoalHg
                \/ cons1 (\(SC.Positive mdp) ->
                        JumpedTooEarly (TooEarlyPoints mdp))
                \/ cons2 (\(SC.Positive spp) (SC.Positive jtg) ->
                        Jumped (SecondsPerPoint $ MkQuantity spp) (JumpedTheGun $ MkQuantity jtg))
                \/ cons2 (\(SC.Positive spp) (SC.Positive jtg) ->
                        JumpedNoGoal (SecondsPerPoint $ MkQuantity spp) (JumpedTheGun $ MkQuantity jtg))

instance Monad m => SC.Serial m (PtTest Pg) where
    series = decDepth $ mkPtTest <$> penalty <~> jumps <~> others <~> cons4 mkParts
        where
            mkPtTest :: SitRep Pg -> [PointPenalty] -> [PointPenalty] -> Points -> PtTest Pg
            mkPtTest a b c d = PtTest a b c d

            mkParts
                (SC.Positive d)
                (SC.Positive l)
                (SC.Positive t)
                (SC.Positive a) =

                Points
                    { reach = LinearPoints $ d / 2
                    , effort = DifficultyPoints $ d / 2
                    , distance = DistancePoints d
                    , leading = LeadingPoints l
                    , time = TimePoints t
                    , arrival = ArrivalPoints a
                    }

            jumps =
                cons1 (pure . PenaltyPoints)
                \/ cons1 (pure . PenaltyFraction)
                \/ cons1 (pure . PenaltyReset)

            others =
                cons1 (pure . PenaltyPoints)
                \/ cons1 (pure . PenaltyFraction)
                \/ cons1 (pure . PenaltyReset)

            penalty =
                cons0 NominalPg
                \/ cons0 NoGoalPg
                \/ cons1 (\(SC.Positive lts) -> Early (LaunchToStartPoints lts))

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

instance QC.Arbitrary (PtTest Hg) where
    arbitrary = do
        penalty <-
            QC.oneof
                [ return NominalHg
                , return NoGoalHg
                , do
                    (QC.Positive mdp) <- arbitrary
                    return $ JumpedTooEarly (TooEarlyPoints $ fromInteger mdp)
                , do
                    (QC.Positive spp) <- arbitrary
                    (QC.Positive jtg) <- arbitrary
                    return $ Jumped (SecondsPerPoint $ MkQuantity spp) (JumpedTheGun $ MkQuantity jtg)
                , do
                    (QC.Positive spp) <- arbitrary
                    (QC.Positive jtg) <- arbitrary
                    return $ JumpedNoGoal (SecondsPerPoint $ MkQuantity spp) (JumpedTheGun $ MkQuantity jtg)
                ]

        (PointParts parts) <- arbitrary
        jumps <-
            QC.oneof
                [ PenaltyPoints <$> arbitrary
                , PenaltyFraction <$> arbitrary
                , PenaltyReset <$> arbitrary
                ]

        others <-
            QC.oneof
                [ PenaltyPoints <$> arbitrary
                , PenaltyFraction <$> arbitrary
                , PenaltyReset <$> arbitrary
                ]

        return $ PtTest penalty [jumps] [others] parts

instance QC.Arbitrary (PtTest Pg) where
    arbitrary = do
        penalty <-
            QC.oneof
                [ return NominalPg
                , return NoGoalPg
                , do
                    (QC.Positive lts) <- arbitrary
                    return $ Early (LaunchToStartPoints $ fromInteger lts)
                ]

        (PointParts parts) <- arbitrary

        jumps <-
            QC.oneof
                [ PenaltyPoints <$> arbitrary
                , PenaltyFraction <$> arbitrary
                , PenaltyReset <$> arbitrary
                ]

        others <-
            QC.oneof
                [ PenaltyPoints <$> arbitrary
                , PenaltyFraction <$> arbitrary
                , PenaltyReset <$> arbitrary
                ]

        return $ PtTest penalty [jumps] [others] parts

-- | Stopped time from announced time.
newtype StopTimeTest a = StopTimeTest (StopTime a) deriving Show

instance Monad m => SC.Serial m (StopTimeTest Hg) where
    series = StopTimeTest <$>
                cons2 (\(SC.Positive i) (SC.Positive t) ->
                    InterGateStop (StartGateInterval i) (AnnouncedTime t))
                \/ cons1 (\(SC.Positive t) ->
                    (SingleGateStop (AnnouncedTime t)))

instance Monad m => SC.Serial m (StopTimeTest Pg) where
    series = StopTimeTest <$>
        cons2 (\(SC.Positive sb) (SC.Positive t) ->
            (ScoreBackStop (ScoreBackTime (MkQuantity sb)) (AnnouncedTime t)))

instance QC.Arbitrary (StopTimeTest Hg) where
    arbitrary = StopTimeTest <$>
        QC.oneof
            [ do
                (QC.Positive i) <- arbitrary
                (QC.Positive t) <- arbitrary
                return $ InterGateStop (StartGateInterval i) (AnnouncedTime t)
            , do
                (QC.Positive t) <- arbitrary
                return $ SingleGateStop (AnnouncedTime t)
            ]

instance QC.Arbitrary (StopTimeTest Pg) where
    arbitrary = StopTimeTest <$> do
        (QC.Positive sb) <- arbitrary
        (QC.Positive t) <- arbitrary
        return $ ScoreBackStop (ScoreBackTime (MkQuantity sb)) (AnnouncedTime t)

-- | Can score a stopped task.
newtype StopCanScoreTest a = StopCanScoreTest (CanScoreStopped a) deriving Show

instance Monad m => SC.Serial m (StopCanScoreTest Hg) where
    series = StopCanScoreTest <$>
                cons2 (\(SC.Positive n) (SC.Positive t) ->
                    Womens (PilotsAtEss n) (TaskStopTime t))
                \/ cons2 (\(SC.Positive n) (SC.Positive t) ->
                    GoalOrDuration (PilotsAtEss n) (TaskStopTime t))

instance Monad m => SC.Serial m (StopCanScoreTest Pg) where
    series = StopCanScoreTest <$>
                cons1 (\(SC.Positive t) ->
                    FromGetGo (TaskStopTime t))
                \/ cons2 (\xs (SC.Positive t) ->
                    FromLastStart ((\(SC.Positive x) -> TaskTime $ MkQuantity x) <$> xs) (TaskStopTime t))

instance QC.Arbitrary (StopCanScoreTest Hg) where
    arbitrary = StopCanScoreTest <$>
        QC.oneof
            [ do
                (QC.Positive n) <- arbitrary
                (QC.Positive t) <- arbitrary
                return $ Womens (PilotsAtEss n) (TaskStopTime t)
            , do
                (QC.Positive n) <- arbitrary
                (QC.Positive t) <- arbitrary
                return $ GoalOrDuration (PilotsAtEss n) (TaskStopTime t)
            ]

instance QC.Arbitrary (StopCanScoreTest Pg) where
    arbitrary = StopCanScoreTest <$>
        QC.oneof
            [ do
                (QC.Positive t) <- arbitrary
                return $ FromGetGo (TaskStopTime t)
            , do
                (QC.Positive t) <- arbitrary
                xs <- listOf $ choose (1, 10000)
                return $ FromLastStart ((\x -> TaskTime . MkQuantity $ x % 1) <$> xs) (TaskStopTime t)
            ]

mkReachStats :: [Double] -> ReachToggle ReachStats

mkReachStats [] =
    let x =
            ReachStats
                { max = FlownMax zero
                , mean = FlownMean zero
                , stdDev = FlownStdDev zero
                }

    in ReachToggle{extra = x, flown = x}

mkReachStats xs =
    let (xsMean, xsVariance) = Stats.meanVariance $ V.fromList xs
        x =
            ReachStats
                { max = FlownMax . MkQuantity $ maximum xs
                , mean = FlownMean $ MkQuantity xsMean
                , stdDev = FlownStdDev . MkQuantity $ sqrt xsVariance
                }

    in ReachToggle{extra = x, flown = x}

-- | Stopped task validity.
newtype StopValidityTest =
    StopValidityTest
        (
            ( PilotsFlying
            , PilotsAtEss
            , PilotsLanded
            , PilotsFlying
            , LaunchToEss (Quantity Double [u| km |])
            )
        ,
            ReachToggle ReachStats
        )
        deriving Show

instance Monad m => SC.Serial m StopValidityTest where
    series = decDepth $ curry StopValidityTest <$> x <~> cons1 mkReachStats
        where
            x = cons4 (\(SC.NonNegative notLanded)
                        (SC.NonNegative atEss)
                        (SC.NonNegative landed)
                        (SC.Positive d) ->
                            ( PilotsFlying (notLanded + atEss + landed)
                            , PilotsAtEss $ fromIntegral atEss
                            , PilotsLanded $ fromIntegral landed
                            , PilotsFlying notLanded
                            , LaunchToEss $ MkQuantity d))

instance QC.Arbitrary StopValidityTest where
    arbitrary = StopValidityTest <$> do
        (QC.NonNegative notLanded) <- arbitrary
        (QC.NonNegative atEss) <- arbitrary
        (QC.NonNegative landed) <- arbitrary
        (QC.Positive d) <- arbitrary
        xs <- listOf $ choose (1, 10000)

        let x =
                ( PilotsFlying (notLanded + atEss + landed)
                , PilotsAtEss $ fromIntegral atEss
                , PilotsLanded $ fromIntegral landed
                , PilotsFlying notLanded
                , LaunchToEss $ MkQuantity d
                )

        return (x, mkReachStats xs)

-- | Stopped task, score time window.
newtype StopWindowTest = StopWindowTest ( TaskType
                                        , StartGates
                                        , TaskStopTime
                                        , [TaskTime]
                                        ) deriving Show

instance Monad m => SC.Serial m StopWindowTest where
    series = (\(a, (b, (c, d))) -> StopWindowTest (a, b, c, d)) <$>
        (cons0 RaceToGoal \/ cons0 ElapsedTime)
        SC.><
        cons1 (\(SC.NonNegative x) -> StartGates x)
        SC.><
        cons1 (\(SC.NonNegative x) -> TaskStopTime x)
        SC.><
        cons1 (\xs -> (\(SC.NonNegative x) -> TaskTime $ MkQuantity x) <$> xs)

instance QC.Arbitrary StopWindowTest where
    arbitrary = StopWindowTest <$> do
        taskType <- oneof [ return RaceToGoal, return ElapsedTime ]
        (QC.NonNegative gates) <- arbitrary
        (QC.NonNegative stop) <- arbitrary
        xs <- listOf $ choose (1, 10000)
        return ( taskType
               , StartGates gates
               , TaskStopTime stop
               , (\x -> TaskTime . MkQuantity $ x % 1) <$> xs
               )

-- | Stopped task, apply glide.
newtype StopGlideTest = StopGlideTest ( GlideRatio
                                      , AltitudeAboveGoal
                                      , StoppedTrack
                                      ) deriving Show

instance Monad m => SC.Serial m StopGlideTest where
    series = (\(a, (b, c)) -> StopGlideTest (a, b, c)) <$>
        cons1 (\(SC.NonNegative x) -> GlideRatio x)
        SC.><
        cons1 (\(SC.NonNegative x) -> AltitudeAboveGoal x)
        SC.><
        cons1 (StoppedTrack .
                zipWith
                    (\t (SC.NonNegative d) -> (TaskTime $ MkQuantity t, DistanceToGoal d))
                    [1 .. ])

instance QC.Arbitrary StopGlideTest where
    arbitrary = StopGlideTest <$> do
        (QC.NonNegative glide) <- arbitrary
        (QC.NonNegative altitude) <- arbitrary
        xs <- listOf $ choose (1, 10000)
        return ( GlideRatio glide
               , AltitudeAboveGoal altitude
               , StoppedTrack $
                   zipWith
                       (\t d -> (TaskTime $ MkQuantity t, DistanceToGoal $ d % 1))
                       [1 .. ]
                       xs
               )
