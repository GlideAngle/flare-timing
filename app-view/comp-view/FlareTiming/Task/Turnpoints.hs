module FlareTiming.Task.Turnpoints (tableTurnpoints) where

import Reflex.Dom
import qualified Data.Text as T (pack)

import Data.Flight.Types (Task, getSpeedSection)
import qualified FlareTiming.Turnpoint as TP (getName)

tableTurnpoints
    :: MonadWidget t m
    => Dynamic t Task
    -> m ()
tableTurnpoints x = do
    let xs = getSpeedSection <$> x
    let zs = (fmap . fmap) (T.pack . TP.getName) xs
    _ <- elClass "table" "table" $
            el "thead" $ do
                el "tr" $ do
                    el "th" $ text "No"
                    el "th" $ text "Id"
                    el "th" $ text "Radius"
                    el "th" $ text "Latitude"
                    el "th" $ text "Longitude"

                simpleList zs (\z ->
                    el "tr" $ do
                        el "td" $ text "-"
                        el "td" $ dynText z
                        el "td" $ text "-"
                        el "td" $ text "-"
                        el "td" $ text "-")
    return ()
