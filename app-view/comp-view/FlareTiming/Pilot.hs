module FlareTiming.Pilot
    ( showPilotId
    , showPilotName
    , showPilot
    , rowPilot
    , rowDfNt
    , rowDfNtReach
    , rowPenal
    ) where

import Reflex.Dom
import qualified Data.Text as T (Text, pack)
import Data.Maybe (fromMaybe)
import Data.List (find)
import Text.Printf (printf)
import Data.Time.LocalTime (TimeZone)

import WireTypes.Route (TaskLength(..), TaskDistance(..))
import WireTypes.Pilot
    ( Pilot(..), PilotId(..), PilotName(..)
    , AwardedDistance(..), AwardedVelocity(..), DfNoTrackPilot(..)
    )
import WireTypes.Point (PointPenalty(..), ReachToggle(..))
import WireTypes.Comp (UtcOffset(..))
import FlareTiming.Time (showT, timeZone)

rowPilot
    :: MonadWidget t m
    => Dynamic t Pilot
    -> m ()
rowPilot x = do
    let td = el "td" . dynText
    el "tr" $ do
        td $ showPilotId <$> x
        td $ showPilotName <$> x

showReach :: TaskLength -> AwardedDistance -> T.Text
showReach TaskLength{taskRoute = TaskDistance td} (AwardedDistance ad)
    | ad == 0 = ""
    | otherwise = T.pack . printf "%.3f km" $ td * ad

showSs :: TimeZone -> AwardedVelocity -> T.Text
showSs tz AwardedVelocity{ss = Just t} = showT tz t
showSs _ _ = ""

showEs :: TimeZone -> AwardedVelocity -> T.Text
showEs tz AwardedVelocity{es = Just t} = showT tz t
showEs _ _ = ""

rowDfNt
    :: MonadWidget t m
    => Dynamic t UtcOffset
    -> Dynamic t (Maybe TaskLength)
    -> Dynamic t DfNoTrackPilot
    -> m ()
rowDfNt utcOffset ln' pd = do
    let tz' = timeZone <$> utcOffset
    dyn_ $ ffor3 tz' ln' pd (\tz ln DfNoTrackPilot{pilot = p, awardedReach = d, awardedVelocity = v} ->
        el "tr" $ do
            el "td" . text $ showPilotId p
            el "td" . text $ showPilotName p
            elClass "td" "td-awarded-start" . text $ showSs tz v
            elClass "td" "td-awarded-end" . text $ showEs tz v
            elClass "td" "td-awarded-reach" . text . fromMaybe "" $ showReach <$> ln <*> (extra <$> d))

rowDfNtReach
    :: MonadWidget t m
    => Dynamic t (Maybe TaskLength)
    -> Int
    -> Dynamic t DfNoTrackPilot
    -> m ()
rowDfNtReach ln' i pd = do
    dyn_ $ ffor2 ln' pd (\ln DfNoTrackPilot{pilot = p, awardedReach = d} ->
        el "tr" $ do
            el "td" . text . T.pack $ show i
            el "td" . text $ showPilotName p
            elClass "td" "td-awarded-reach" . text . fromMaybe "" $ showReach <$> ln <*> (extra <$> d))

rowPenal
    :: MonadWidget t m
    => Dynamic t (Pilot, [PointPenalty], String)
    -> m ()
rowPenal ppp = do
    let td = elClass "td" "td-penalty" . text . T.pack . printf "%.3f"
    dyn_ $ ffor ppp (\(pilot, ps, reason) -> el "tr" $ do
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
                el "td" $ text ""
        el "td" . text $ T.pack reason)

showPilotId :: Pilot -> T.Text
showPilotId (Pilot (PilotId x, _)) = T.pack x

showPilotName :: Pilot -> T.Text
showPilotName (Pilot (_, PilotName x)) = T.pack x

showPilot :: Pilot -> T.Text
showPilot (Pilot (PilotId x, PilotName y)) = T.pack $ printf "%03s-%s" x y
