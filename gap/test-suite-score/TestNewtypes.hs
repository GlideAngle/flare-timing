{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# lANGUAGE PatternSynonyms #-}
module TestNewtypes where

-- NOTE: Avoid orphan instance warnings with these newtypes.

import Prelude hiding (seq)
import Data.Ratio ((%))
import Data.List (sortBy)
import Test.SmallCheck.Series as SC
import Test.Tasty.QuickCheck as QC

import Flight.Gap.Ratio (pattern (:%))
import Flight.Score
    ( Lw(..)
    , Aw(..)
    , GoalRatio(..)
    , NominalGoal(..)
    , MinimumDistance(..)
    , NominalDistance(..)
    , DistanceRatio(..)
    , DistanceWeight(..)
    , LeadingWeight(..)
    , ArrivalWeight(..)
    , LaunchValidity(..)
    , TimeValidity(..)
    , DistanceValidity(..)
    , PilotsAtEss(..)
    , PositionAtEss(..)
    , BestTime(..)
    , PilotTime(..)
    , BestDistance(..)
    , PilotDistance(..)
    , TaskTime(..)
    , DistanceToEss(..)
    , Leg(..)
    , LcTrack
    , LcSeq(..)
    , LcPoint(..)
    , TaskDeadline(..)
    , LengthOfSs(..)
    , LaunchToSssPoints(..)
    , MinimumDistancePoints(..)
    , SecondsPerPoint(..)
    , JumpedTheGun(..)
    , Hg
    , Pg
    , Penalty(..)
    , TaskPointParts(..)
    , StopTime(..)
    , ScoreBackTime(..)
    , AnnouncedTime(..)
    , StartGateInterval(..)
    , NumberInGoalAtStop(..)
    , CanScoreStopped(..)
    , TaskStopTime(..)
    , PilotsLaunched(..)
    , PilotsLandedBeforeStop(..)
    , DistanceLaunchToEss(..)
    , DistanceFlown(..)
    , TaskType(..)
    , StartGates(..)
    , GlideRatio(..)
    , AltitudeAboveGoal(..)
    , DistanceToGoal(..)
    , StoppedTrack(..)
    )

import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Normal (Normal(..), NormalSum(..))
import Flight.Units()

-- | Nominal goal.
newtype NgTest = NgTest NominalGoal deriving Show

instance Monad m => SC.Serial m NgTest where
    series = cons1 $ \(Normal x) -> NgTest (NominalGoal x)

instance QC.Arbitrary NgTest where
    arbitrary = arbitrary >>= \(Normal x) -> return $ NgTest (NominalGoal x)

-- | Nominal distance.
newtype NdTest = NdTest NominalDistance deriving Show

instance Monad m => SC.Serial m NdTest where
    series = cons1 $ \(SC.NonNegative x) -> NdTest (NominalDistance x)

instance QC.Arbitrary NdTest where
    arbitrary = arbitrary >>= \(QC.NonNegative x) -> return $ NdTest (NominalDistance x)

-- | Leading weight.
newtype LwTest = LwTest (Lw Rational) deriving Show

instance Monad m => SC.Serial m LwTest where
    series = LwTest <$> (xs \/ ys)
        where
            xs = cons1 $ \(Normal x) -> LwHg (DistanceWeight x)
            ys = cons1 $ \(Normal x) -> LwPg (DistanceWeight x)

instance QC.Arbitrary LwTest where
    arbitrary = LwTest <$> arb
        where
        arb = do
            (Normal r) <- arbitrary
            QC.oneof $ return <$> [ LwHg (DistanceWeight r)
                                  , LwPgZ (DistanceRatio r)
                                  , LwPg (DistanceWeight r)
                                  ]

-- | Arrival weight for paragliding when goal ratio is zero.
newtype AwTestPgZ = AwTestPgZ (Aw ()) deriving Show

instance Monad m => SC.Serial m AwTestPgZ where
    series = cons0 $ AwTestPgZ AwPg

instance QC.Arbitrary AwTestPgZ where
    arbitrary = arbitrary >>= \() -> return $ AwTestPgZ AwPg

-- | Arrival weight.
newtype AwTest = AwTest (Aw Rational) deriving Show

instance Monad m => SC.Serial m AwTest where
    series = cons1 $ \(Normal x) -> AwTest (AwHg (DistanceWeight x))

instance QC.Arbitrary AwTest where
    arbitrary = do
        (Normal x) <- arbitrary
        return $ AwTest (AwHg (DistanceWeight x))

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
newtype TvTest = TvTest (LaunchValidity, TimeValidity, DistanceValidity) deriving Show

instance Monad m => SC.Serial m TvTest where
    series =
        cons1 $ \(NormalSum (x, y, z)) ->
             TvTest (LaunchValidity x, TimeValidity y, DistanceValidity z)

instance QC.Arbitrary TvTest where
    arbitrary = do
        (NormalSum (x, y, z)) <- arbitrary
        return $ TvTest (LaunchValidity x, TimeValidity y, DistanceValidity z)

-- | Arrival fraction
newtype AfTest = AfTest (PilotsAtEss, PositionAtEss) deriving Show

instance Monad m => SC.Serial m AfTest where
    series =
        cons1 $ \(Normal (rank :% n)) ->
             AfTest (PilotsAtEss n, PositionAtEss $ max 1 rank)

instance QC.Arbitrary AfTest where
    arbitrary = do
        (Normal (rank :% n)) <- arbitrary
        return $ AfTest (PilotsAtEss n, PositionAtEss $ max 1 rank)

-- | Speed fraction
newtype SfTest = SfTest (BestTime, PilotTime) deriving Show

instance Monad m => SC.Serial m SfTest where
    series =
        cons1 $ \(Normal (n :% d)) ->
             SfTest (BestTime (d % 1), PilotTime $ (d + n) % 1)

instance QC.Arbitrary SfTest where
    arbitrary = do
        (Normal (n :% d)) <- arbitrary
        return $ SfTest (BestTime (d % 1), PilotTime $ (d + n) % 1)

-- | Linear fraction 
newtype LfTest =
    LfTest
        ( BestDistance (Quantity Double [u| km |])
        , PilotDistance (Quantity Double [u| km |])
        )
    deriving Show

instance Monad m => SC.Serial m LfTest where
    series =
        cons1 $ \(Normal ((n :% d) :: Rational))  ->
             LfTest
                 ( BestDistance . MkQuantity . fromIntegral $ d + n
                 , PilotDistance . MkQuantity . fromIntegral $ d
                 )

instance QC.Arbitrary LfTest where
    arbitrary = do
        (Normal ((n :% d) :: Rational)) <- arbitrary
        return . LfTest $
            ( BestDistance . MkQuantity . fromIntegral $ d + n
            , PilotDistance . MkQuantity . fromIntegral $ d
            )

-- | Difficulty fraction
newtype DfTest =
    DfTest
        ( MinimumDistance
        , BestDistance (Quantity Double [u| km |])
        , [PilotDistance (Quantity Double [u| km |])]
        )
    deriving Show

mkDfTest :: [Int] -> DfTest
mkDfTest xs =
    case toRational <$> sortBy (flip compare) (abs <$> xs) of
        [] ->
            DfTest
                ( MinimumDistance . MkQuantity $ 0
                , BestDistance . MkQuantity $ 0
                , []
                )

        ys ->
            DfTest
                ( MinimumDistance . MkQuantity . fromRational $ minimum ys
                , BestDistance . MkQuantity . fromRational $ maximum ys
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
         [] -> LcSeq [] Nothing
         ys ->
             LcSeq{seq = pts, extra = Nothing}
             where
                 ts = TaskTime <$> [1 .. ]
                 ds = DistanceToEss <$> ys
                 pts =
                     zipWith
                         (\t d -> LcPoint{leg = RaceLeg 0, mark = t, togo = d})
                         ts
                         ds

mkLcCleanTest :: Rational -> [Int] -> LcCleanTest
mkLcCleanTest len xs =
    LcCleanTest (LengthOfSs len, mkLcTrack xs)

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
    LcTest (TaskDeadline deadline, LengthOfSs len, mkLcTrack <$> xs)

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
newtype PtTest a = PtTest (Maybe (Penalty a), TaskPointParts) deriving Show

instance Monad m => SC.Serial m (PtTest Hg) where
    series = decDepth $ mkPtTest <$> penalty <~> cons4 mkParts
        where
            mkPtTest :: Maybe (Penalty Hg) -> TaskPointParts -> PtTest Hg
            mkPtTest a b = PtTest (a, b)

            mkParts
                (SC.Positive d)
                (SC.Positive l)
                (SC.Positive t)
                (SC.Positive a) =

                TaskPointParts { distance = d, leading = l, time = t, arrival = a }

            penalty =
                cons0 Nothing
                \/ cons1 (\(SC.Positive mdp) ->
                        Just $ JumpedTooEarly (MinimumDistancePoints mdp))
                \/ cons2 (\(SC.Positive spp) (SC.Positive jtg) ->
                        Just $ Jumped (SecondsPerPoint spp) (JumpedTheGun jtg))
                \/ cons2 (\(SC.Positive spp) (SC.Positive jtg) ->
                        Just $ JumpedNoGoal (SecondsPerPoint spp) (JumpedTheGun jtg))
                \/ cons0 (Just NoGoalHg)

instance Monad m => SC.Serial m (PtTest Pg) where
    series = decDepth $ mkPtTest <$> penalty <~> cons4 mkParts
        where
            mkPtTest :: Maybe (Penalty Pg) -> TaskPointParts -> PtTest Pg
            mkPtTest a b = PtTest (a, b)

            mkParts
                (SC.Positive d)
                (SC.Positive l)
                (SC.Positive t)
                (SC.Positive a) =

                TaskPointParts { distance = d, leading = l, time = t, arrival = a }

            penalty =
                cons0 Nothing
                \/ cons1 (\(SC.Positive lts) -> Just $ Early (LaunchToSssPoints lts))

newtype PointParts = PointParts TaskPointParts deriving Show

instance QC.Arbitrary PointParts where
    arbitrary = do
        (QC.Positive d) <- arbitrary
        (QC.Positive l) <- arbitrary
        (QC.Positive t) <- arbitrary
        (QC.Positive a) <- arbitrary
        return $ PointParts TaskPointParts { distance = d, leading = l, time = t, arrival = a }

instance QC.Arbitrary (PtTest Hg) where
    arbitrary = do
        penalty <-
            QC.oneof
                [ return Nothing
                , do
                    x <-
                        QC.oneof
                            [ do
                                (QC.Positive mdp) <- arbitrary
                                return $ JumpedTooEarly (MinimumDistancePoints mdp)
                            , do
                                (QC.Positive spp) <- arbitrary
                                (QC.Positive jtg) <- arbitrary
                                return $ Jumped (SecondsPerPoint spp) (JumpedTheGun jtg)
                            , do
                                (QC.Positive spp) <- arbitrary
                                (QC.Positive jtg) <- arbitrary
                                return $ JumpedNoGoal (SecondsPerPoint spp) (JumpedTheGun jtg)
                            , return NoGoalHg
                            ]

                    return $ Just x
                ] 

        (PointParts parts) <- arbitrary

        return $ PtTest (penalty, parts)

instance QC.Arbitrary (PtTest Pg) where
    arbitrary = do
        penalty <-
            QC.oneof
                [ return Nothing
                , do
                    x <- QC.oneof
                            [ do
                                (QC.Positive lts) <- arbitrary
                                return $ Early (LaunchToSssPoints lts)
                            , return NoGoalPg
                            ]

                    return $ Just x
                ] 

        (PointParts parts) <- arbitrary

        return $ PtTest (penalty, parts)

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
            (ScoreBackStop (ScoreBackTime sb) (AnnouncedTime t)))

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
        return $ ScoreBackStop (ScoreBackTime sb) (AnnouncedTime t)

-- | Can score a stopped task.
newtype StopCanScoreTest a = StopCanScoreTest (CanScoreStopped a) deriving Show

instance Monad m => SC.Serial m (StopCanScoreTest Hg) where
    series = StopCanScoreTest <$>
                cons2 (\(SC.Positive n) (SC.Positive t) ->
                    Womens (NumberInGoalAtStop n) (TaskStopTime t))
                \/ cons2 (\(SC.Positive n) (SC.Positive t) ->
                    GoalOrDuration (NumberInGoalAtStop n) (TaskStopTime t))

instance Monad m => SC.Serial m (StopCanScoreTest Pg) where
    series = StopCanScoreTest <$>
                cons1 (\(SC.Positive t) ->
                    FromGetGo (TaskStopTime t))
                \/ cons2 (\xs (SC.Positive t) ->
                    FromLastStart ((\(SC.Positive x) -> TaskTime (x % 1)) <$> xs) (TaskStopTime t))

instance QC.Arbitrary (StopCanScoreTest Hg) where
    arbitrary = StopCanScoreTest <$>
        QC.oneof
            [ do
                (QC.Positive n) <- arbitrary
                (QC.Positive t) <- arbitrary
                return $ Womens (NumberInGoalAtStop n) (TaskStopTime t)
            , do
                (QC.Positive n) <- arbitrary
                (QC.Positive t) <- arbitrary
                return $ GoalOrDuration (NumberInGoalAtStop n) (TaskStopTime t)
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
                return $ FromLastStart ((\x -> TaskTime (x % 1)) <$> xs) (TaskStopTime t)
            ]

-- | Stopped task validity.
newtype StopValidityTest = StopValidityTest ( PilotsLaunched
                                            , PilotsLandedBeforeStop
                                            , DistanceLaunchToEss
                                            , [DistanceFlown]
                                            ) deriving Show

instance Monad m => SC.Serial m StopValidityTest where
    series = StopValidityTest <$>
                cons4 (\(SC.NonNegative notLanded)
                        (SC.NonNegative landed)
                        (SC.Positive d)
                        
                        xs->
                            ( PilotsLaunched (notLanded + landed)
                            , PilotsLandedBeforeStop landed
                            , DistanceLaunchToEss d
                            , (\x -> DistanceFlown $ x % 1) <$> xs
                            ))

instance QC.Arbitrary StopValidityTest where
    arbitrary = StopValidityTest <$> do
        (QC.NonNegative notLanded) <- arbitrary
        (QC.NonNegative landed) <- arbitrary
        (QC.Positive d) <- arbitrary
        xs <- listOf $ choose (1, 10000)
        return ( PilotsLaunched (notLanded + landed)
               , PilotsLandedBeforeStop landed
               , DistanceLaunchToEss d
               , (\x -> DistanceFlown $ x % 1) <$> xs
               )

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
        cons1 (\xs -> (\(SC.NonNegative x) -> TaskTime x) <$> xs)

instance QC.Arbitrary StopWindowTest where
    arbitrary = StopWindowTest <$> do
        taskType <- oneof [ return RaceToGoal, return ElapsedTime ]
        (QC.NonNegative gates) <- arbitrary
        (QC.NonNegative stop) <- arbitrary
        xs <- listOf $ choose (1, 10000)
        return ( taskType
               , StartGates gates
               , TaskStopTime stop
               , (\x -> TaskTime $ x % 1) <$> xs
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
                    (\t (SC.NonNegative d) -> (TaskTime t, DistanceToGoal d))
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
                       (\t d -> (TaskTime t, DistanceToGoal $ d % 1))
                       [1 .. ]
                       xs
               )
