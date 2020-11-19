module FlareTiming.Pilot
    ( showPilotId
    , showPilotName
    , showPilot
    , hashIdHyphenPilot
    , rowPilot
    , rowDfNt
    , rowDfNtReach
    , classOfEarlyStart
    , rowPenalJump
    , rowPenalAuto
    , rowPenal
    ) where

import Reflex.Dom
import qualified Data.Text as T (Text, pack)
import Data.Maybe (fromMaybe)
import Text.Printf (printf)
import Data.Time.LocalTime (TimeZone)

import WireTypes.Route (TaskLength(..), TaskDistance(..))
import WireTypes.Pilot
    ( Pilot(..), PilotId(..), PilotName(..)
    , AwardedDistance(..), AwardedVelocity(..), DfNoTrackPilot(..)
    )
import WireTypes.Penalty
    (PenaltySeqs(..), pprEffectiveAdd, pprNthAdd, pprEffectiveMul, pprEffectiveReset)
import WireTypes.Point (ReachToggle(..), JumpedTheGun(..), showJumpedTheGunTime)
import WireTypes.Comp (UtcOffset(..), JumpTheGunLimit(..))
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
            elClass "td" "td-pid" . text $ showPilotId p
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
            elClass "td" "td-pid" . text . T.pack $ show i
            el "td" . text $ showPilotName p
            elClass "td" "td-awarded-reach" . text . fromMaybe "" $ showReach <$> ln <*> (extra <$> d))

classOfEarlyStart :: JumpTheGunLimit -> Maybe JumpedTheGun -> T.Text
classOfEarlyStart (JumpTheGunLimit e) =
    let c = "td-start-early" in
    maybe
        c
        (\(JumpedTheGun j) ->
            if j > e
               then c <> " " <> "jumped-too-early"
               else c)

rowPenalJump
    :: MonadWidget t m
    => Int
    -> Dynamic t JumpTheGunLimit
    -> Dynamic t (Pilot, Maybe PenaltySeqs, PenaltySeqs, Maybe JumpedTheGun)
    -> m ()
rowPenalJump dp earliest ppp = do
    dyn_ $ ffor ppp (\(pilot, pRaw, PenaltySeqs{adds = pPoints, resets = pResets}, jump) -> el "tr" $ do

        let classEarly = ffor earliest (flip classOfEarlyStart $ jump)

        elClass "td" "td-pid" . text . showPilotId $ pilot
        el "td" . text . showPilotName $ pilot
        elDynClass "td" classEarly . text $ showJumpedTheGunTime jump

        if null pResets
            then el "td" $ text ""
            else tdPointPenalty $ pprEffectiveReset dp pResets

        if null pPoints
            then do
                el "td" $ text ""
                el "td" $ text ""
                el "td" $ text ""

            else do
                maybe
                    (do
                        el "td" $ text ""
                        el "td" $ text "")
                    (\PenaltySeqs{adds} -> do
                        tdPointPenalty $ pprNthAdd dp 0 adds
                        tdPointPenalty $ pprNthAdd dp 1 adds)
                    pRaw

                tdPointPenalty $ pprEffectiveAdd dp pPoints)

tdPointPenalty :: MonadWidget t m => T.Text -> m ()
tdPointPenalty = elClass "td" "td-penalty" . text

rowPenalAuto
    :: MonadWidget t m
    => Int
    -> Dynamic t (Pilot, PenaltySeqs, String)
    -> m ()
rowPenalAuto dp ppp = do
    dyn_ $ ffor ppp (\(pilot, PenaltySeqs{adds = pp}, reason) -> el "tr" $ do
        elClass "td" "td-pid" . text . showPilotId $ pilot
        el "td" . text . showPilotName $ pilot
        if null pp
            then el "td" $ text ""
            else tdPointPenalty $ pprEffectiveAdd dp pp

        el "td" . text $ T.pack reason)

rowPenal
    :: MonadWidget t m
    => Int
    -> Dynamic t (Pilot, PenaltySeqs, String)
    -> m ()
rowPenal dp ppp = do
    dyn_ $ ffor ppp (\(pilot, PenaltySeqs{muls = p, adds = pp}, reason) -> el "tr" $ do
        elClass "td" "td-pid" . text . showPilotId $ pilot
        el "td" . text . showPilotName $ pilot
        case (p, pp) of
            ([], []) -> do
                el "td" $ text ""
                el "td" $ text ""
            (_, []) -> do
                tdPointPenalty $ pprEffectiveMul dp p
                el "td" $ text ""
            ([], _) -> do
                el "td" $ text ""
                tdPointPenalty $ pprEffectiveAdd dp pp
            _ -> do
                tdPointPenalty $ pprEffectiveMul dp p
                tdPointPenalty $ pprEffectiveAdd dp pp
        el "td" . text $ T.pack reason)

showPilotId :: Pilot -> T.Text
showPilotId (Pilot (PilotId x, _)) = T.pack x

showPilotName :: Pilot -> T.Text
showPilotName (Pilot (_, PilotName x)) = T.pack x

showPilot :: Int -> Pilot -> T.Text
showPilot _ (Pilot (PilotId "", _)) = ""
showPilot w _ | w <= 0 = ""
showPilot w (Pilot (PilotId x, PilotName y)) =
    T.pack $ printf "%0*s-%s" w x y

hashIdHyphenPilot :: Int -> T.Text
hashIdHyphenPilot w =
    T.pack $ replicate w '#' ++ "-Pilot"
