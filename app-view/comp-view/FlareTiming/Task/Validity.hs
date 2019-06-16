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
import WireTypes.Cross (FlyingSection)
import WireTypes.Route (TaskDistance(..))
import WireTypes.Reach (TrackReach(..), BolsterStats(..))
import WireTypes.Pilot (Pilot(..))
import WireTypes.Comp (MinimumDistance(..), Task(..), UtcOffset(..), TaskStop(..))
import qualified WireTypes.Point as Norm (NormBreakdown(..))
import FlareTiming.Task.Validity.Widget (spacer)
import FlareTiming.Task.Validity.Launch (viewLaunch, launchWorking)
import FlareTiming.Task.Validity.Time (viewTime, timeWorking)
import FlareTiming.Task.Validity.Distance(viewDistance, distanceWorking)
import FlareTiming.Task.Validity.Stop (viewStop, stopWorking)
import FlareTiming.Task.Validity.Task (viewTask, taskWorking)

hookWorking
    :: Vy.Validity
    -> Vy.Validity
    -> ValidityWorking
    -> ValidityWorking
    -> BolsterStats
    -> TaskDistance
    -> Int
    -> T.Text
hookWorking
    v
    vN
    ValidityWorking{launch = l, distance = d, time = t}
    ValidityWorking{launch = lN, distance = dN}
    r td landed =
    taskWorking "task-working" v
    <> taskWorking "task-working-norm" vN
    <> launchWorking "launch-working" v l
    <> launchWorking "launch-working-norm" vN lN
    <> distanceWorking "distance-working" v d
    <> distanceWorking "distance-working-norm" vN dN
    <> timeWorking v t
    <> stopWorking d t r td landed

viewValidity
    :: MonadWidget t m
    => Dynamic t UtcOffset
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
    -> Dynamic t (Maybe TaskDistance)
    -> Dynamic t (Maybe [(Pilot, FlyingSection UTCTime)])
    -> Dynamic t [(Pilot, Norm.NormBreakdown)]
    -> m ()
viewValidity
    utcOffset free task
    vy vyNorm
    vw vwNorm
    reachStats bonusStats
    reach bonusReach
    td
    flyingTimes
    sEx = do

    let (landedByStop, stillFlying) =
            splitDynPure
            $ ffor2 task (fromMaybe [] <$> flyingTimes) (\Task{stopped} ft ->
                maybe
                    (ft, [])
                    (\TaskStop{retroactive = t} ->
                        partition (maybe False ((< t) . snd) . snd) ft)
                    stopped)

    _ <- dyn $ ffor2 vy vyNorm (\vy' vyNorm' ->
            dyn $ ffor2 vw vwNorm (\vw' vwNorm' ->
            dyn $ ffor2 reachStats bonusStats (\reachStats' bonusStats' ->
            dyn $ ffor3 td landedByStop sEx (\td' lo sEx' ->
                case (vy', vyNorm', vw', vwNorm', reachStats', bonusStats', td') of
                    (Nothing, _, _, _, _, _, _) -> text "Loading validity ..."
                    (_, Nothing, _, _, _, _, _) -> text "Loading expected validity from FS ..."
                    (_, _, Nothing, _, _, _, _) -> text "Loading validity workings ..."
                    (_, _, _, Nothing, _, _, _) -> text "Loading expected validity workings from FS ..."
                    (_, _, _, _, Nothing, _, _) -> text "Loading reach stats ..."
                    (_, _, _, _, _, Nothing, _) -> text "Loading bonus reach stats ..."
                    (_, _, _, _, _, _, Nothing) -> text "Loading stopped task distance ..."
                    (Just v@Vy.Validity{task = dq, launch = lv, distance = dv, time = tv}
                        , Just vN, Just w, Just wN, Just rStats, Just bStats, Just d) -> do
                        elAttr
                            "a"
                            (("class" =: "button") <> ("onclick" =: hookWorking v vN w wN rStats d (length lo)))
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
                            free
                            v vN
                            w wN
                            rStats
                            bStats
                            (fromMaybe [] <$> reach)
                            (fromMaybe [] <$> bonusReach)
                            landedByStop
                            stillFlying
                            sEx'

                        spacer
                        return ()))))

    return ()
