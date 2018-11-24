module FlareTiming.Task.Turnpoints (tableTurnpoints) where

import Reflex.Dom
import qualified Data.Text as T (Text, pack)

import WireTypes.Comp
    (Task(..), RawZone(..), SpeedSection, getAllRawZones, getSpeedSection)
import qualified FlareTiming.Turnpoint as TP

tableTurnpoints
    :: MonadWidget t m
    => Dynamic t Task
    -> m ()
tableTurnpoints x = do
    let ss = getSpeedSection <$> x
    let zs = fmap (zip [1..]) $ getAllRawZones <$> x

    _ <- elClass "table" "table" $
            el "thead" $ do
                el "tr" $ do
                    el "th" $ text "No"
                    el "th" $ text "Id"
                    el "th" $ text "Radius"
                    el "th" $ text "Latitude"
                    el "th" $ text "Longitude"

                simpleList zs (row ss)
    return ()

row
    :: MonadWidget t m
    => Dynamic t SpeedSection
    -> Dynamic t (Integer, RawZone)
    -> m ()
row ss iz = do
    let c = zipDynWith rowColor ss $ fst <$> iz
    let z = snd <$> iz

    elDynClass "tr" c $ do
        el "td" $ dynText $ T.pack . show . fst <$> iz
        el "td" . dynText $ TP.getName <$> z
        el "td" . dynText $ TP.getRadius <$> z
        el "td" . dynText $ TP.getLat <$> z
        el "td" . dynText $ TP.getLng <$> z

rowColor :: SpeedSection -> Integer -> T.Text
rowColor Nothing _ = ""
rowColor (Just (ss, es)) ii =
    if | ss == ii -> "has-background-success"
       | es == ii -> "has-background-danger"
       | otherwise -> ""
