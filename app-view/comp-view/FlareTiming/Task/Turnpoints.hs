module FlareTiming.Task.Turnpoints (tableTurnpoints) where

import Reflex.Dom
import qualified Data.Text as T (pack)

import Data.Flight.Types (Task(..), Zones(..), RawZone(..))
import qualified FlareTiming.Turnpoint as TP

tableTurnpoints
    :: MonadWidget t m
    => Dynamic t Task
    -> m ()
tableTurnpoints x = do
    let zs = fmap (zip [1..]) $ getZones <$> x

    _ <- elClass "table" "table" $
            el "thead" $ do
                el "tr" $ do
                    el "th" $ text "No"
                    el "th" $ text "Id"
                    el "th" $ text "Radius"
                    el "th" $ text "Latitude"
                    el "th" $ text "Longitude"

                simpleList zs row
    return ()

getZones :: Task -> [RawZone]
getZones (Task _ Zones{raw} _) = raw

row
    :: MonadWidget t m
    => Dynamic t (Integer, RawZone)
    -> m ()
row iz = do
    let z = snd <$> iz
    el "tr" $ do
        el "td" $ dynText $ T.pack . show . fst <$> iz
        el "td" . dynText $ TP.getName <$> z
        el "td" . dynText $ TP.getRadius <$> z
        el "td" . dynText $ TP.getLat <$> z
        el "td" . dynText $ TP.getLng <$> z
