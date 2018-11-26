module FlareTiming.Task.Quality.Allocation (tableAllocation) where

import Reflex.Dom
import qualified Data.Text as T (Text)

import WireTypes.Track.Point
    ( Points(..)
    , TaskPoints(..)
    , showDistancePoints
    , showLinearPoints
    , showDifficultyPoints
    , showArrivalPoints
    , showTimePoints
    , showTaskPoints
    )

tableAllocation
    :: MonadWidget t m
    => Dynamic t (Maybe Points)
    -> Dynamic t (Maybe TaskPoints)
    -> m ()
tableAllocation x tp = do
    let td = elClass "td" "has-text-right"

    _ <- elClass "table" "table" $
            el "thead" $ do
                el "tr" $ do
                    el "th" $ text ""
                    el "th" $ text "Points"
                el "tr" $ do
                    el "td" $ text "Distance Points"
                    td . dynText $ getDistance <$> x
                el "tr" $ do
                    el "td" $ text "Reach (Linear Distance) Points"
                    td . dynText $ getLinear <$> x
                el "tr" $ do
                    el "td" $ text "Effort (Difficulty) Points"
                    td . dynText $ getDifficulty <$> x
                el "tr" $ do
                    el "td" $ text "Arrival Points"
                    td . dynText $ getArrival <$> x
                el "tr" $ do
                    el "td" $ text "Time Points"
                    td . dynText $ getTime <$> x
                elClass "tr" "has-background-light" $ do
                    el "td" $ text "Task Points"
                    td . dynText $ getTask <$> tp

    return ()

getDistance :: Maybe Points -> T.Text
getDistance = maybe "" (showDistancePoints . distance)

getLinear :: Maybe Points -> T.Text
getLinear = maybe "" (showLinearPoints . reach)

getDifficulty :: Maybe Points -> T.Text
getDifficulty = maybe "" (showDifficultyPoints . effort)

getArrival :: Maybe Points -> T.Text
getArrival = maybe "" (showArrivalPoints . arrival)

getTime :: Maybe Points -> T.Text
getTime = maybe "" (showTimePoints . time)

getTask :: Maybe TaskPoints -> T.Text
getTask = maybe "" showTaskPoints
