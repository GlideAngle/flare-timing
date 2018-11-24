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

showVyLaunch :: LaunchValidity -> T.Text
showVyLaunch (LaunchValidity v) = T.pack . show $ v

showVyDistance :: DistanceValidity -> T.Text
showVyDistance (DistanceValidity v) = T.pack . show $ v

showVyTime :: TimeValidity -> T.Text
showVyTime (TimeValidity v) = T.pack . show $ v

showVyTask :: TaskValidity -> T.Text
showVyTask (TaskValidity v) = T.pack . show $ v

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
