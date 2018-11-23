module FlareTiming.Task.Absent (tableAbsent) where

import Reflex.Dom
import qualified Data.Text as T (Text, pack)

import Data.Flight.Types
    (Task(..), Pilot(..), PilotId(..), PilotName(..), getAbsent)

tableAbsent
    :: MonadWidget t m
    => Dynamic t Task
    -> m ()
tableAbsent x = do
    let xs = getAbsent <$> x

    _ <- elClass "table" "table" $
            el "thead" $ do
                el "tr" $ do
                    el "th" $ text "Id"
                    el "th" $ text "Name"

                simpleList xs row
    return ()

row
    :: MonadWidget t m
    => Dynamic t Pilot
    -> m ()
row x = do
    el "tr" $ do
        el "td" . dynText $ showPilotId <$> x
        el "td" . dynText $ showPilotName <$> x

showPilotId :: Pilot -> T.Text
showPilotId (Pilot (PilotId x, _)) = T.pack x

showPilotName :: Pilot -> T.Text
showPilotName (Pilot (_, PilotName x)) = T.pack x
