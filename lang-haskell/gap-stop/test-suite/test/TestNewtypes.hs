module TestNewtypes where

-- NOTE: Avoid orphan instance warnings with these newtypes.

import Data.Ratio ((%))
import Test.SmallCheck.Series as SC
import Test.Tasty.QuickCheck as QC
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Test.QuickCheck.Instances ()

import "flight-gap-math" Flight.Score (Hg, Pg)
import "flight-gap-lead" Flight.Score (TaskTime(..))
import "flight-gap-allot" Flight.Score
import "flight-gap-stop" Flight.Score

import Flight.Units()

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
