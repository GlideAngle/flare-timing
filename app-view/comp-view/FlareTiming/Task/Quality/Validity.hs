module FlareTiming.Task.Quality.Validity (tableValidity) where

import Reflex.Dom
import qualified Data.Text as T (Text)

import WireTypes.Track.Point
    ( Validity(..)
    , showLaunchValidity
    , showDistanceValidity
    , showTimeValidity
    , showTaskValidity
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
                    el "td" . dynText $ getLaunch <$> x
                el "tr" $ do
                    el "td" $ text "Distance"
                    el "td" . dynText $ getDistance <$> x
                el "tr" $ do
                    el "td" $ text "Time"
                    el "td" . dynText $ getTime <$> x
                elClass "tr" "has-background-light" $ do
                    el "td" $ text "Task"
                    el "td" . dynText $ getTask <$> x
    return ()

getLaunch :: Maybe Validity -> T.Text
getLaunch = maybe "" showLaunchValidity . (fmap launch)

getDistance :: Maybe Validity -> T.Text
getDistance = maybe "" showDistanceValidity. (fmap distance)

getTime :: Maybe Validity -> T.Text
getTime = maybe "" showTimeValidity . (fmap time)

getTask :: Maybe Validity -> T.Text
getTask = maybe "" showTaskValidity . (fmap task)
