module FlareTiming.Pilot.Row (row) where

import Reflex.Dom
import qualified Data.Text as T (Text, pack)

import Data.Flight.Types (Pilot(..), PilotId(..), PilotName(..))

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
