module FlareTiming.Plot.Time.Table (tableSpeed) where

import Reflex.Dom
import qualified Data.Text as T (Text)
import Data.Map (Map)
import qualified Data.Map.Strict as Map

import qualified WireTypes.Fraction as Frac (Fractions(..))
import WireTypes.Fraction (showSpeedFrac, showSpeedFracDiff)
import qualified WireTypes.Point as Norm (NormBreakdown(..))
import WireTypes.Speed (TrackSpeed(..), PilotTime(..))
import qualified WireTypes.Speed as Speed (TrackSpeed(..))
import WireTypes.Pilot (Pilot(..), pilotIdsWidth)
import WireTypes.Point (StartGate)
import FlareTiming.Pilot (showPilot, hashIdHyphenPilot)
import FlareTiming.Time (showHmsForHours, showHours)
import FlareTiming.Task.Score.Show (showPilotTime, showPilotTimeDiff)

tableSpeed
    :: MonadWidget t m
    => Dynamic t [StartGate]
    -> Dynamic t [(Pilot, Norm.NormBreakdown)]
    -> Dynamic t [(Pilot, TrackSpeed)]
    -> m ()
tableSpeed sgs sEx xs = do
    let w = ffor xs (pilotIdsWidth . fmap fst)
    _ <- elClass "table" "table is-striped" $ do
            el "thead" $ do
                el "tr" $ do
                    elAttr "th" ("colspan" =: "2")
                        $ text ""
                    elAttr "th" ("colspan" =: "2" <> "class" =: "th-norm time-or-pace") . dynText
                        $ ffor sgs (\case [] -> "Pace"; _ -> "Time")
                    elAttr "th" ("colspan" =: "3" <> "class" =: "th-time-frac")
                        $ text "Fraction"

                    el "th" $ text ""

                el "tr" $ do
                    el "th" $ text "H.hhh"
                    el "th" $ text "HH:MM:SS"
                    elClass "th" "th-norm th-norm-pace" $ text "✓"
                    elClass "th" "th-norm th-time-diff" $ text "Δ"

                    el "th" $ text ""
                    elClass "th" "th-norm" $ text "✓"
                    elClass "th" "th-norm" $ text "Δ"
                    el "th" . dynText $ ffor w hashIdHyphenPilot

                    return ()

            _ <- dyn $ ffor sEx (\sEx' -> do
                    let mapN = Map.fromList sEx'

                    el "tbody" $
                        simpleList xs (uncurry (rowSpeed w mapN) . splitDynPure))

            return ()
    return ()

rowSpeed
    :: MonadWidget t m
    => Dynamic t Int
    -> Map Pilot Norm.NormBreakdown
    -> Dynamic t Pilot
    -> Dynamic t TrackSpeed
    -> m ()
rowSpeed w mapN p ts = do
    (yTime, yTimeDiff, yFrac, yFracDiff) <- sample . current
                $ ffor2 p ts (\pilot TrackSpeed{time, frac} ->
                    case Map.lookup pilot mapN of
                        Just
                            Norm.NormBreakdown
                                { timeElapsed = timeN
                                , fractions = Frac.Fractions{time = fracN}
                                } ->
                            ( showPilotTime time
                            , maybe "" (flip showPilotTimeDiff time) timeN

                            , showSpeedFrac fracN
                            , showSpeedFracDiff fracN frac
                            )

                        _ -> ("", "", "", ""))

    el "tr" $ do
        el "td" . dynText $ showHr . Speed.time <$> ts
        el "td" . dynText $ showHms . Speed.time <$> ts
        elClass "td" "td-norm td-norm-pace" . text $ yTime
        elClass "td" "td-norm td-time-diff" . text $ yTimeDiff
        el "td" . dynText $ showSpeedFrac . frac <$> ts
        elClass "td" "td-norm" . text $ yFrac
        elClass "td" "td-norm" . text $ yFracDiff
        el "td" . dynText $ ffor2 w p showPilot

        return ()

showHr :: PilotTime -> T.Text
showHr (PilotTime x) = showHours x

showHms :: PilotTime -> T.Text
showHms (PilotTime x) = showHmsForHours x
