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

showDistance :: DistancePoints -> T.Text
showDistance (DistancePoints p) = T.pack . show $ p

showLinear :: LinearPoints -> T.Text
showLinear (LinearPoints p) = T.pack . show $ p

showDifficulty :: DifficultyPoints -> T.Text
showDifficulty (DifficultyPoints p) = T.pack . show $ p

showArrival :: ArrivalPoints -> T.Text
showArrival (ArrivalPoints p) = T.pack . show $ p

showTime :: TimePoints -> T.Text
showTime (TimePoints p) = T.pack . show $ p

showTask :: TaskPoints -> T.Text
showTask (TaskPoints p) = T.pack . show $ p

getDistance :: Maybe Points -> T.Text
getDistance = maybe "" (showDistance . distance)

getLinear :: Maybe Points -> T.Text
getLinear = maybe "" (showLinear . reach)

getDifficulty :: Maybe Points -> T.Text
getDifficulty = maybe "" (showDifficulty . effort)

getArrival :: Maybe Points -> T.Text
getArrival = maybe "" (showArrival . arrival)

getTime :: Maybe Points -> T.Text
getTime = maybe "" (showTime . time)

getTask :: Maybe TaskPoints -> T.Text
getTask = maybe "" showTask
