module FlareTiming.Pilot
    ( showPilotId
    , showPilotName
    , rowPilot
    , rowPenal
    ) where

import Reflex.Dom
import qualified Data.Text as T (Text, pack)
import Data.List (find)
import Text.Printf (printf)

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
rowPenal ppp = do
    let td = el "td" . text . T.pack . printf "%.3f"
    dyn_ $ ffor ppp (\(pilot, ps) -> el "tr" $ do
        let p =
                find
                    (\case PenaltyFraction _ -> True; PenaltyPoints _ -> False)
                    ps

        let pp =
                find
                    (\case PenaltyFraction _ -> False; PenaltyPoints _ -> True)
                    ps

        el "td" . text . showPilotId $ pilot
        el "td" . text . showPilotName $ pilot
        case (p, pp) of
            (Just (PenaltyFraction x), Nothing) -> do
                td x
                el "td" $ text ""
            (Nothing, Just (PenaltyPoints y)) -> do
                el "td" $ text ""
                td y
            (Just (PenaltyFraction x), Just (PenaltyPoints y)) -> do
                td x
                td y
            _ -> do
                el "td" $ text ""
                el "td" $ text "")

showPilotId :: Pilot -> T.Text
showPilotId (Pilot (PilotId x, _)) = T.pack x

showPilotName :: Pilot -> T.Text
showPilotName (Pilot (_, PilotName x)) = T.pack x
