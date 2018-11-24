module FlareTiming.Task.Quality.Weight (tableWeight) where

import Reflex.Dom
import qualified Data.Text as T (Text, pack)

import WireTypes.Track.Point
    ( Weights(..)
    , DistanceWeight(..)
    , LeadingWeight(..)
    , ArrivalWeight(..)
    , TimeWeight(..)
    , Allocation(..)
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
                    el "td" . dynText $ getDistance <$> x
                el "tr" $ do
                    el "td" $ text "Leading Weight"
                    el "td" . dynText $ getLeading <$> x
                el "tr" $ do
                    el "td" $ text "Arrival Weight"
                    el "td" . dynText $ getArrival <$> x
                el "tr" $ do
                    el "td" $ text "Time Weight"
                    el "td" . dynText $ getTime <$> x

    return ()

showDistance :: DistanceWeight -> T.Text
showDistance (DistanceWeight p) = T.pack . show $ p

showLeading :: LeadingWeight -> T.Text
showLeading (LeadingWeight p) = T.pack . show $ p

showArrival :: ArrivalWeight -> T.Text
showArrival (ArrivalWeight p) = T.pack . show $ p

showTime :: TimeWeight -> T.Text
showTime (TimeWeight p) = T.pack . show $ p

getDistance :: Maybe Weights -> T.Text
getDistance = maybe "" (showDistance . distance)

getLeading :: Maybe Weights -> T.Text
getLeading = maybe "" (showLeading . leading)

getArrival :: Maybe Weights -> T.Text
getArrival = maybe "" (showArrival . arrival)

getTime :: Maybe Weights -> T.Text
getTime = maybe "" (showTime . time)
