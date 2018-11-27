module FlareTiming.Task.Quality.Weight (tableWeight) where

import Reflex.Dom
import qualified Data.Text as T (Text)

import WireTypes.Track.Point
    ( Weights(..)
    , showDistanceWeight
    , showArrivalWeight
    , showTimeWeight
    , showLeadingWeight
    )

tableWeight
    :: MonadWidget t m
    => Dynamic t (Maybe Weights)
    -> m ()
tableWeight x = do

    _ <- elClass "table" "table" $
            el "thead" $ do
                el "tr" $ do
                    el "th" $ text ""
                    el "th" $ text "Weight"
                el "tr" $ do
                    el "td" $ text "Distance Weight"
                    el "td" . dynText $ showMaybeDistanceWeight <$> x
                el "tr" $ do
                    el "td" $ text "Leading Weight"
                    el "td" . dynText $ showMaybeLeadingWeight <$> x
                el "tr" $ do
                    el "td" $ text "Arrival Weight"
                    el "td" . dynText $ showMaybeArrivalWeight <$> x
                el "tr" $ do
                    el "td" $ text "Time Weight"
                    el "td" . dynText $ showMaybeTimeWeight <$> x

    return ()

showMaybeDistanceWeight :: Maybe Weights -> T.Text
showMaybeDistanceWeight = maybe "" (showDistanceWeight . distance)

showMaybeLeadingWeight :: Maybe Weights -> T.Text
showMaybeLeadingWeight = maybe "" (showLeadingWeight . leading)

showMaybeArrivalWeight :: Maybe Weights -> T.Text
showMaybeArrivalWeight = maybe "" (showArrivalWeight . arrival)

showMaybeTimeWeight :: Maybe Weights -> T.Text
showMaybeTimeWeight = maybe "" (showTimeWeight . time)
