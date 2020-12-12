{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
module FlareTiming.Plot.Reach.TableBonus (tablePilotReachBonus) where

import Reflex.Dom
import Data.Maybe (fromMaybe)
import Data.Map (Map)
import qualified Data.Map.Strict as Map

import WireTypes.Fraction (showReachFrac)
import WireTypes.Reach (TrackReach(..))
import WireTypes.Pilot (Pilot(..), pilotIdsWidth)
import WireTypes.Point (showPilotDistance, showPilotDistanceDiff)
import FlareTiming.Pilot (showPilot, hashIdHyphenPilot)
import FlareTiming.Plot.Event (rowClass)

tablePilotReachBonus
    :: MonadWidget t m
    => Dynamic t [(Pilot, TrackReach)]
    -> Dynamic t [(Pilot, TrackReach)]
    -> Dynamic t [Pilot]
    -> m (Event t Pilot)
tablePilotReachBonus xs xsBonus select = do
    let tdFoot = elAttr "td" ("colspan" =: "6")
    let foot = el "tr" . tdFoot . text
    let w = ffor xs (pilotIdsWidth . fmap fst)

    ev :: Event _ (Event _ Pilot) <- elClass "table" "table is-striped" $ do
            el "thead" $ do
                el "tr" $ do
                    elAttr "th" ("colspan" =: "3")
                        $ text "Reach (km)"
                    elAttr "th" (("colspan" =: "2") <> ("class" =: "th-reach-frac"))
                        $ text "Fraction"

                    el "th" . dynText $ ffor w hashIdHyphenPilot

                    return ()

            el "thead" $ do
                el "tr" $ do
                    elClass "th" "th-plot-reach" $ text "Flown"
                    elClass "th" "th-plot-reach-bonus" $ text "Scored †"
                    elClass "th" "th-plot-reach-bonus-diff" $ text "Δ"

                    elClass "th" "th-plot-frac" $ text "Flown"
                    elClass "th" "th-plot-frac-bonus" $ text "Scored ‡"

                    el "th" $ text ""

                    return ()

            ev <- dyn $ ffor xsBonus (\br -> do
                    let mapR = Map.fromList br
                    ePilots <- el "tbody" $ simpleList xs (uncurry (rowReachBonus w select mapR) . splitDynPure)
                    return $ switchDyn $ leftmost <$> ePilots)

            el "tfoot" $ do
                foot "† Reach as scored."
                foot "Δ Altitude bonus reach."
                foot "‡ The fraction of reach points as scored."

            return ev
    switchHold never ev

rowReachBonus
    :: MonadWidget t m
    => Dynamic t Int
    -> Dynamic t [Pilot]
    -> Map Pilot TrackReach
    -> Dynamic t Pilot
    -> Dynamic t TrackReach
    -> m (Event t Pilot)
rowReachBonus w select mapR p tr = do
    pilot <- sample $ current p

    (eReach, eReachDiff, eFrac) <- sample . current
            $ ffor tr (\TrackReach{reach = reachF} ->
                fromMaybe ("", "", "") $ do
                    TrackReach{reach = reachE, frac = fracE} <- Map.lookup pilot mapR

                    return
                        ( showPilotDistance 1 $ reachE
                        , showPilotDistanceDiff 1 reachF reachE
                        , showReachFrac fracE
                        ))

    (eRow, _) <- elDynClass' "tr" (ffor2 p select rowClass) $ do
        elClass "td" "td-plot-reach" . dynText $ showPilotDistance 1 . reach <$> tr
        elClass "td" "td-plot-reach-bonus" $ text eReach
        elClass "td" "td-plot-reach-bonus-diff" $ text eReachDiff

        elClass "td" "td-plot-frac" . dynText $ showReachFrac . frac <$> tr
        elClass "td" "td-plot-frac-bonus" $ text eFrac

        elClass "td" "td-pilot" . dynText $ ffor2 w p showPilot

        return ()

    return $ const pilot <$> domEvent Click eRow
