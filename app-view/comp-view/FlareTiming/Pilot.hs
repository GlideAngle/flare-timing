module FlareTiming.Pilot
    ( showPilotId
    , showPilotName
    , rowPilot
    , rowPenal
    ) where

import Reflex.Dom
import qualified Data.Text as T (Text, pack)

import WireTypes.Pilot (Pilot(..), PilotId(..), PilotName(..))
import WireTypes.Point (PointPenalty(..))

rowPilot
    :: MonadWidget t m
    => Dynamic t Pilot
    -> m ()
rowPilot x = do
    let td = el "td" . dynText
    el "tr" $ do
        td $ showPilotId <$> x
        td $ showPilotName <$> x

rowPenal
    :: MonadWidget t m
    => Dynamic t (Pilot, [PointPenalty])
    -> m ()
rowPenal x = do
    let pilot = fst <$> x
    let td = el "td" . dynText
    el "tr" $ do
        td $ showPilotId <$> pilot
        td $ showPilotName <$> pilot
        el "td" $ text ""
        el "td" $ text ""

showPilotId :: Pilot -> T.Text
showPilotId (Pilot (PilotId x, _)) = T.pack x

showPilotName :: Pilot -> T.Text
showPilotName (Pilot (_, PilotName x)) = T.pack x
