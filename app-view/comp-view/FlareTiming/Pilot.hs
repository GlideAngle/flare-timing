module FlareTiming.Pilot
    ( showPilotId
    , showPilotName
    , rowPilot
    ) where

import Reflex.Dom
import qualified Data.Text as T (Text, pack)

import WireTypes.Pilot (Pilot(..), PilotId(..), PilotName(..))

rowPilot
    :: MonadWidget t m
    => Dynamic t Pilot
    -> m ()
rowPilot x = do
    let td = el "td" . dynText
    el "tr" $ do
        td $ showPilotId <$> x
        td $ showPilotName <$> x

showPilotId :: Pilot -> T.Text
showPilotId (Pilot (PilotId x, _)) = T.pack x

showPilotName :: Pilot -> T.Text
showPilotName (Pilot (_, PilotName x)) = T.pack x