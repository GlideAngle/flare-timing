module FlareTiming.Task.Validity
    ( tableValidity
    , tableAllocation
    ) where

import Reflex.Dom
import qualified Data.Text as T (Text, pack)

import WireTypes.Track.Point
    ( Validity(..)
    , TaskValidity(..)
    , LaunchValidity(..)
    , DistanceValidity(..)
    , TimeValidity(..)
    , Allocation(..)
    , GoalRatio(..)
    , DistancePoints(..)
    , LinearPoints(..)
    , DifficultyPoints(..)
    , ArrivalPoints(..)
    , TimePoints(..)
    , TaskPoints(..)
    , Points(..)
    )

tableValidity
    :: MonadWidget t m
    => Dynamic t (Maybe Validity)
    -> m ()
tableValidity x = do

    _ <- elClass "table" "table" $
            el "thead" $ do
                el "tr" $ do
                    el "th" $ text ""
                    el "th" $ text "Validity"
                el "tr" $ do
                    el "td" $ text "Launch"
                    el "td" . dynText $ getVyLaunch <$> x
                el "tr" $ do
                    el "td" $ text "Distance"
                    el "td" . dynText $ getVyDistance <$> x
                el "tr" $ do
                    el "td" $ text "Time"
                    el "td" . dynText $ getVyTime <$> x
                elClass "tr" "has-background-light" $ do
                    el "td" $ text "Task"
                    el "td" . dynText $ getVyTask <$> x
    return ()

tableAllocation
    :: MonadWidget t m
    => Dynamic t (Maybe Allocation)
    -> m ()
tableAllocation x = do

    _ <- elClass "table" "table" $
            el "thead" $ do
                el "tr" $ do
                    el "th" $ text ""
                    el "th" $ text "Points"
                el "tr" $ do
                    el "td" $ text "Goal Ratio"
                    el "td" . dynText $ getGoalRatio <$> x
                el "tr" $ do
                    el "td" $ text "Distance Points"
                    el "td" . dynText $ getPtDistance <$> x
                el "tr" $ do
                    el "td" $ text "Reach (Linear Distance) Points"
                    el "td" . dynText $ getPtLinear <$> x
                el "tr" $ do
                    el "td" $ text "Effort (Difficulty) Points"
                    el "td" . dynText $ getPtDifficulty <$> x
                el "tr" $ do
                    el "td" $ text "Arrival Points"
                    el "td" . dynText $ getPtArrival <$> x
                el "tr" $ do
                    el "td" $ text "Time Points"
                    el "td" . dynText $ getPtTime <$> x
                el "tr" $ do
                    el "td" $ text "Task Points"
                    el "td" . dynText $ getPtTask <$> x

    return ()

showVyLaunch :: LaunchValidity -> T.Text
showVyLaunch (LaunchValidity v) = T.pack . show $ v

showVyDistance :: DistanceValidity -> T.Text
showVyDistance (DistanceValidity v) = T.pack . show $ v

showVyTime :: TimeValidity -> T.Text
showVyTime (TimeValidity v) = T.pack . show $ v

showVyTask :: TaskValidity -> T.Text
showVyTask (TaskValidity v) = T.pack . show $ v

showGoalRatio :: GoalRatio -> T.Text
showGoalRatio (GoalRatio gr) = T.pack . show $ gr

showPtDistance :: DistancePoints -> T.Text
showPtDistance (DistancePoints p) = T.pack . show $ p

showPtLinear :: LinearPoints -> T.Text
showPtLinear (LinearPoints p) = T.pack . show $ p

showPtDifficulty :: DifficultyPoints -> T.Text
showPtDifficulty (DifficultyPoints p) = T.pack . show $ p

showPtArrival :: ArrivalPoints -> T.Text
showPtArrival (ArrivalPoints p) = T.pack . show $ p

showPtTime :: TimePoints -> T.Text
showPtTime (TimePoints p) = T.pack . show $ p

showPtTask :: TaskPoints -> T.Text
showPtTask (TaskPoints p) = T.pack . show $ p

getVyLaunch :: Maybe Validity -> T.Text
getVyLaunch =
    maybe "" showVyLaunch . (fmap launch)

getVyDistance :: Maybe Validity -> T.Text
getVyDistance =
    maybe "" showVyDistance . (fmap (\Validity{distance = x} -> x))

getVyTime :: Maybe Validity -> T.Text
getVyTime =
    maybe "" showVyTime . (fmap (\Validity{time = x} -> x))

getVyTask :: Maybe Validity -> T.Text
getVyTask =
    maybe "" showVyTask . (fmap task)

getGoalRatio :: Maybe Allocation -> T.Text
getGoalRatio =
    maybe "" showGoalRatio . (fmap goalRatio)

getPtDistance :: Maybe Allocation -> T.Text
getPtDistance =
    maybe "" (showPtDistance . \Points{distance = x} -> x) . (fmap points)

getPtLinear :: Maybe Allocation -> T.Text
getPtLinear =
    maybe "" (showPtLinear . \Points{reach = x} -> x) . (fmap points)

getPtDifficulty :: Maybe Allocation -> T.Text
getPtDifficulty =
    maybe "" (showPtDifficulty . \Points{effort = x} -> x) . (fmap points)

getPtArrival :: Maybe Allocation -> T.Text
getPtArrival =
    maybe "" (showPtArrival . \Points{arrival = x} -> x) . (fmap points)

getPtTime :: Maybe Allocation -> T.Text
getPtTime =
    maybe "" (showPtTime . \Points{time = x} -> x) . (fmap points)

getPtTask :: Maybe Allocation -> T.Text
getPtTask =
    maybe "" showPtTask . (fmap taskPoints)
