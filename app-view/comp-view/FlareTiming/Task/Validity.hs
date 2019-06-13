module FlareTiming.Task.Validity (viewValidity) where

import Data.Maybe (fromMaybe)
import Data.Time.Clock (UTCTime)
import Reflex
import Reflex.Dom
import qualified Data.Text as T (Text)
import Data.List (partition)

import qualified WireTypes.Validity as Vy (Validity(..))
import WireTypes.ValidityWorking (ValidityWorking(..))
import WireTypes.Cross (FlyingSection)
import WireTypes.Route (TaskDistance(..))
import WireTypes.Reach (TrackReach(..), ReachStats(..))
import WireTypes.Pilot (Pilot(..))
import WireTypes.Comp (Task(..), UtcOffset(..), TaskStop(..))
import FlareTiming.Task.Validity.Widget (spacer)
import FlareTiming.Task.Validity.Launch (viewLaunch, launchWorking)
import FlareTiming.Task.Validity.Time (viewTime, timeWorking)
import FlareTiming.Task.Validity.Distance(viewDistance, distanceWorking)
import FlareTiming.Task.Validity.Stop (viewStop, stopWorking)
import FlareTiming.Task.Validity.Task (viewTask, taskWorking)

hookWorking
    :: Vy.Validity
    -> ValidityWorking
    -> ReachStats
    -> TaskDistance
    -> Int
    -> T.Text
hookWorking v ValidityWorking{launch = l, distance = d, time = t} r td landed =
    taskWorking v
    <> launchWorking v l
    <> distanceWorking v d
    <> timeWorking v t
    <> stopWorking d t r td landed

viewValidity
    :: MonadWidget t m
    => Dynamic t UtcOffset
    -> Dynamic t Task
    -> Dynamic t (Maybe Vy.Validity)
    -> Dynamic t (Maybe Vy.Validity)
    -> Dynamic t (Maybe ValidityWorking)
    -> Dynamic t (Maybe ValidityWorking)
    -> Dynamic t (Maybe ReachStats)
    -> Dynamic t (Maybe ReachStats)
    -> Dynamic t (Maybe [(Pilot, TrackReach)])
    -> Dynamic t (Maybe [(Pilot, TrackReach)])
    -> Dynamic t (Maybe TaskDistance)
    -> Dynamic t (Maybe [(Pilot, FlyingSection UTCTime)])
    -> m ()
viewValidity
    utcOffset task
    vy vyNorm
    vw vwNorm
    reachStats bonusStats
    reach bonusReach
    td flyingTimes = do
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
            dyn $ ffor2 td landedByStop (\td' lo ->
                case (vy', vyNorm', vw', vwNorm', reachStats', bonusStats', td') of
                    (Nothing, _, _, _, _, _, _) -> text "Loading validity ..."
                    (_, Nothing, _, _, _, _, _) -> text "Loading expected validity from FS ..."
                    (_, _, Nothing, _, _, _, _) -> text "Loading validity workings ..."
                    (_, _, _, Nothing, _, _, _) -> text "Loading expected validity workings from FS ..."
                    (_, _, _, _, Nothing, _, _) -> text "Loading reach stats ..."
                    (_, _, _, _, _, Nothing, _) -> text "Loading bonus reach stats ..."
                    (_, _, _, _, _, _, Nothing) -> text "Loading stopped task distance ..."
                    (Just v, Just vN, Just w, Just wN, Just rStat, Just _, Just d) -> do
                        elAttr
                            "a"
                            (("class" =: "button") <> ("onclick" =: hookWorking v w rStat d (length lo)))
                            (text "Show Working")

                        spacer
                        viewTask v vN w
                        spacer
                        viewLaunch v vN w wN
                        spacer
                        viewDistance v vN w wN
                        spacer
                        viewTime v vN w wN
                        spacer

                        viewStop
                            utcOffset
                            v vN
                            w wN
                            reachStats
                            bonusStats
                            (fromMaybe [] <$> reach)
                            (fromMaybe [] <$> bonusReach)
                            d
                            landedByStop
                            stillFlying

                        spacer
                        return ()))))

    return ()
