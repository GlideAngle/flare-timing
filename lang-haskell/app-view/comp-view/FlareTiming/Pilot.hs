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
import Data.List (find)
import Text.Printf (printf)
import Data.Time.LocalTime (TimeZone)

import WireTypes.Route (TaskLength(..), TaskDistance(..))
import WireTypes.Pilot
    ( Pilot(..), PilotId(..), PilotName(..)
    , AwardedDistance(..), AwardedVelocity(..), DfNoTrackPilot(..)
    )
import WireTypes.Point (PointPenalty(..), ReachToggle(..), JumpedTheGun(..), showJumpedTheGunTime)
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
    => Dynamic t JumpTheGunLimit
    -> Dynamic t (Pilot, [PointPenalty], Maybe JumpedTheGun)
    -> m ()
rowPenalJump earliest ppp = do
    let tdPoint = elClass "td" "td-penalty" . text . T.pack . printf "%+.3f"
    let tdReset = elClass "td" "td-penalty" . text . T.pack . printf "%.3f"
    dyn_ $ ffor ppp (\(pilot, ps, jump) -> el "tr" $ do
        let pPoint =
                find
                    (\case PenaltyPoints{} -> True; _ -> False)
                    ps

        let pReset =
                find
                    (\case PenaltyReset{} -> True; _ -> False)
                    ps

        let classEarly = ffor earliest (flip classOfEarlyStart $ jump)

        el "td" . text . showPilotId $ pilot
        el "td" . text . showPilotName $ pilot
        elDynClass "td" classEarly . text $ showJumpedTheGunTime jump

        case pPoint of
            Just (PenaltyPoints y) -> tdPoint $ negate y
            _ -> el "td" $ text ""

        case pReset of
            Just (PenaltyReset y) -> tdReset $ (fromIntegral y :: Double)
            _ -> el "td" $ text "")

rowPenalAuto
    :: MonadWidget t m
    => Dynamic t (Pilot, [PointPenalty], String)
    -> m ()
rowPenalAuto ppp = do
    let td = elClass "td" "td-penalty" . text . T.pack . printf "%+.3f"
    dyn_ $ ffor ppp (\(pilot, ps, reason) -> el "tr" $ do
        let pp =
                find
                    (\case PenaltyPoints{} -> True; _ -> False)
                    ps

        el "td" . text . showPilotId $ pilot
        el "td" . text . showPilotName $ pilot
        case pp of
            Just (PenaltyPoints y) -> td $ negate y
            _ -> el "td" $ text ""

        el "td" . text $ T.pack reason)

rowPenal
    :: MonadWidget t m
    => Dynamic t (Pilot, [PointPenalty], String)
    -> m ()
rowPenal ppp = do
    let td = elClass "td" "td-penalty" . text . T.pack . printf "%+.3f"
    dyn_ $ ffor ppp (\(pilot, ps, reason) -> el "tr" $ do
        let p =
                find
                    (\case PenaltyFraction{} -> True; _ -> False)
                    ps

        let pp =
                find
                    (\case PenaltyPoints{} -> True; _ -> False)
                    ps

        el "td" . text . showPilotId $ pilot
        el "td" . text . showPilotName $ pilot
        case (p, pp) of
            (Just (PenaltyFraction x), Nothing) -> do
                td $ negate x
                el "td" $ text ""
            (Nothing, Just (PenaltyPoints y)) -> do
                el "td" $ text ""
                td $ negate y
            (Just (PenaltyFraction x), Just (PenaltyPoints y)) -> do
                td $ negate x
                td $ negate y
            _ -> do
                el "td" $ text ""
                el "td" $ text ""
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
