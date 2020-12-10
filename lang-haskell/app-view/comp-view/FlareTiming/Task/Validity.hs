module FlareTiming.Task.Validity (viewValidity) where

import Data.Maybe (fromMaybe)
import Data.Time.Clock (UTCTime)
import Reflex
import Reflex.Dom
import qualified Data.Text as T (Text)
import Data.List (partition)

import qualified WireTypes.Validity as Vy
    ( Validity(..)
    , showTaskValidity, showLaunchValidity, showDistanceValidity, showTimeValidity
    )
import WireTypes.ValidityWorking (ValidityWorking(..))
import WireTypes.Route (TaskLength(..))
import WireTypes.Cross (FlyingSection)
import WireTypes.Reach (TrackReach(..), BolsterStats(..))
import WireTypes.Pilot (Pilot(..), DfNoTrack(..))
import WireTypes.Comp (MinimumDistance(..), Task(..), UtcOffset(..), TaskStop(..))
import qualified WireTypes.Point as Alt (AltBreakdown(..))
import FlareTiming.Validity.Widget (spacer)
import FlareTiming.Validity.Launch (viewLaunch, launchWorking)
import FlareTiming.Validity.Time (viewTime, timeWorking)
import FlareTiming.Validity.Distance(viewDistance, distanceWorking)
import FlareTiming.Validity.Stop (viewStop, stopWorking)
import FlareTiming.Validity.Task (viewTask, taskWorking)

hookWorking
    :: Vy.Validity
    -> Vy.Validity
    -> ValidityWorking
    -> ValidityWorking
    -> T.Text
hookWorking
    v
    vN
    ValidityWorking{launch = l, distance = d, time = t, stop = s}
    ValidityWorking{launch = lN, distance = dN, time = tN, stop = sN} =
    taskWorking "task-working" v
    <> taskWorking "task-working-norm" vN
    <> launchWorking "launch-working" v l
    <> launchWorking "launch-working-norm" vN lN
    <> distanceWorking "distance-working" v d
    <> distanceWorking "distance-working-norm" vN dN
    <> timeWorking "time-working" v t
    <> timeWorking "time-working-norm" vN tN
    <> maybe "" (\s' -> stopWorking "stop-working" v s' tN) s
    <> maybe "" (\s' -> stopWorking "stop-working-norm" vN s' tN) sN

viewValidity
    :: MonadWidget t m
    => Dynamic t UtcOffset
    -> Dynamic t (Maybe TaskLength)
    -> Dynamic t MinimumDistance
    -> Dynamic t Task
    -> Dynamic t (Maybe Vy.Validity)
    -> Dynamic t (Maybe Vy.Validity)
    -> Dynamic t (Maybe ValidityWorking)
    -> Dynamic t (Maybe ValidityWorking)
    -> Dynamic t (Maybe BolsterStats)
    -> Dynamic t (Maybe BolsterStats)
    -> Dynamic t (Maybe [(Pilot, TrackReach)])
    -> Dynamic t (Maybe [(Pilot, TrackReach)])
    -> Dynamic t (Maybe [(Pilot, FlyingSection UTCTime)])
    -> Dynamic t DfNoTrack
    -> Dynamic t [(Pilot, Alt.AltBreakdown)]
    -> m ()
viewValidity
    utcOffset ln free task
    vy vyAlt
    vw vwAlt
    reachStats bonusStats
    reach bonusReach
    flyingTimes
    dfNt
    sEx = do

    let (landedByStop, stillFlying) =
            splitDynPure
            $ ffor2 task (fromMaybe [] <$> flyingTimes) (\Task{stopped} ft ->
                maybe
                    (ft, [])
                    (\TaskStop{retroactive = t} ->
                        partition (maybe False ((< t) . snd) . snd) ft)
                    stopped)

    _ <- dyn $ ffor2 vy vyAlt (\vy' vyAlt' ->
            dyn $ ffor2 vw vwAlt (\vw' vwAlt' ->
            dyn $ ffor2 reachStats bonusStats (\reachStats' bonusStats' ->
            dyn $ ffor sEx (\sEx' ->
                case (vy', vyAlt', vw', vwAlt', reachStats', bonusStats') of
                    (Nothing, _, _, _, _, _) -> text "Loading validity ..."
                    (_, Nothing, _, _, _, _) -> text "Loading expected validity from FS ..."
                    (_, _, Nothing, _, _, _) -> text "Loading validity workings ..."
                    (_, _, _, Nothing, _, _) -> text "Loading expected validity workings from FS ..."
                    (_, _, _, _, Nothing, _) -> text "Loading reach stats ..."
                    (_, _, _, _, _, Nothing) -> text "Loading bonus reach stats ..."
                    (Just v@Vy.Validity{task = dq, launch = lv, distance = dv, time = tv}
                        , Just vN, Just w, Just wN, Just rStats, Just bStats) -> do
                        elAttr
                            "a"
                            (("class" =: "button") <> ("onclick" =: hookWorking v vN w wN))
                            (text "Show Working")

                        spacer
                        elClass "div" "tile is-ancestor" $ do
                            elClass "div" "tile is-12" $
                                elClass "div" "tile" $ do
                                    elClass "div" "tile is-parent" $ do
                                        elClass "article" "tile is-child box" $ do
                                            elClass "h2" "title is-4" . text
                                                $ "Task Validity* = " <> Vy.showTaskValidity dq
                                            elClass "div" "content"
                                                $ viewTask v vN w

                                    elClass "div" "tile is-parent" $ do
                                        elClass "article" "tile is-child box" $ do
                                            elClass "h2" "title is-4" $ do
                                                elClass "span" "legend-launch" $ text "▩"
                                                text $ " Launch Validity = " <> Vy.showLaunchValidity lv

                                            elClass "div" "content"
                                                $ viewLaunch v vN w wN

                        elClass "div" "tile is-ancestor" $ do
                            elClass "div" "tile is-12" $
                                elClass "div" "tile" $ do
                                    elClass "div" "tile is-parent" $ do
                                        elClass "article" "tile is-child box" $ do
                                            elClass "h2" "title is-4" $ do
                                                elClass "span" "legend-reach" $ text "▩"
                                                text $ " Distance Validity = " <> Vy.showDistanceValidity dv

                                            elClass "div" "content"
                                                $ viewDistance v vN w wN

                                    elClass "div" "tile is-parent" $ do
                                        elClass "article" "tile is-child box" $ do
                                            elClass "h2" "title is-4" $ do
                                                elClass "span" "legend-time" $ text "▩"
                                                text $ " Time Validity = " <> Vy.showTimeValidity tv

                                            elClass "div" "content"
                                                $ viewTime v vN w wN

                        viewStop
                            utcOffset
                            ln
                            free
                            v vN
                            w wN
                            rStats
                            bStats
                            (fromMaybe [] <$> reach)
                            (fromMaybe [] <$> bonusReach)
                            landedByStop
                            stillFlying
                            dfNt
                            sEx'

                        spacer
                        return ()))))

    return ()
