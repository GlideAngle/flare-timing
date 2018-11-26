module FlareTiming.Task.Quality.Validity (tableValidity) where

import Reflex.Dom
import qualified Data.Text as T (Text, pack)

import WireTypes.Track.Point
    ( Validity(..)
    , TaskValidity(..)
    , LaunchValidity(..)
    , DistanceValidity(..)
    , TimeValidity(..)
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

showLaunch :: LaunchValidity -> T.Text
showLaunch (LaunchValidity v) = T.pack . show $ v

showDistance :: DistanceValidity -> T.Text
showDistance (DistanceValidity v) = T.pack . show $ v

showTime :: TimeValidity -> T.Text
showTime (TimeValidity v) = T.pack . show $ v

showTask :: TaskValidity -> T.Text
showTask (TaskValidity v) = T.pack . show $ v

getLaunch :: Maybe Validity -> T.Text
getLaunch = maybe "" showLaunch . (fmap launch)

getDistance :: Maybe Validity -> T.Text
getDistance = maybe "" showDistance . (fmap distance)

getTime :: Maybe Validity -> T.Text
getTime = maybe "" showTime . (fmap time)

getTask :: Maybe Validity -> T.Text
getTask = maybe "" showTask . (fmap task)
