module FlareTiming.Task.Quality.Allocation (tableAllocation) where

import Reflex.Dom
import qualified Data.Text as T (Text, pack)

import WireTypes.Track.Point
    ( DistancePoints(..)
    , LinearPoints(..)
    , DifficultyPoints(..)
    , ArrivalPoints(..)
    , TimePoints(..)
    , TaskPoints(..)
    , Points(..)
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
                    td . dynText $ getPtDistance <$> x
                el "tr" $ do
                    el "td" $ text "Reach (Linear Distance) Points"
                    td . dynText $ getPtLinear <$> x
                el "tr" $ do
                    el "td" $ text "Effort (Difficulty) Points"
                    td . dynText $ getPtDifficulty <$> x
                el "tr" $ do
                    el "td" $ text "Arrival Points"
                    td . dynText $ getPtArrival <$> x
                el "tr" $ do
                    el "td" $ text "Time Points"
                    td . dynText $ getPtTime <$> x
                elClass "tr" "has-background-light" $ do
                    el "td" $ text "Task Points"
                    td . dynText $ getPtTask <$> tp

    return ()

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

getPtDistance :: Maybe Points -> T.Text
getPtDistance =
    maybe "" (showPtDistance . distance)

getPtLinear :: Maybe Points -> T.Text
getPtLinear =
    maybe "" (showPtLinear . reach)

getPtDifficulty :: Maybe Points -> T.Text
getPtDifficulty =
    maybe "" (showPtDifficulty . effort)

getPtArrival :: Maybe Points -> T.Text
getPtArrival =
    maybe "" (showPtArrival . arrival)

getPtTime :: Maybe Points -> T.Text
getPtTime =
    maybe "" (showPtTime . time)

getPtTask :: Maybe TaskPoints -> T.Text
getPtTask = maybe "" showPtTask
