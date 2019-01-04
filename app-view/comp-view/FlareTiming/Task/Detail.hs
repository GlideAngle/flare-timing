module FlareTiming.Task.Detail (taskDetail) where

import Prelude hiding (map)
import Reflex
import Reflex.Dom
import qualified Data.Text as T (intercalate)

import WireTypes.Pilot (nullPilot)
import WireTypes.Comp (Comp(..), Task(..), getRaceRawZones)
import WireTypes.Route (TaskLength(..), taskLength, taskLegs, showTaskDistance)
import WireTypes.Point (Allocation(..))
import WireTypes.Validity (Validity(..))
import FlareTiming.Comms
    ( getTaskScore, getTaskValidityWorking, getTaskLengthSphericalEdge
    , getTaskPilotDnf, getTaskPilotNyp, getTaskPilotTrack
    )
import FlareTiming.Breadcrumb (crumbTask)
import FlareTiming.Events (IxTask(..))
import FlareTiming.Map.View (viewMap)
import qualified FlareTiming.Turnpoint as TP (getName)
import FlareTiming.Task.Tab (TaskTab(..), tabsTask)
import FlareTiming.Task.Score (tableScore)
import FlareTiming.Task.Turnpoints (tableTask)
import FlareTiming.Task.Absent (tableAbsent)
import FlareTiming.Task.Validity (viewValidity)

taskTileZones
    :: MonadWidget t m
    => Dynamic t Task
    -> Dynamic t (Maybe TaskLength)
    -> m ()
taskTileZones t len = do
    let xs = getRaceRawZones <$> t
    let zs = (fmap . fmap) TP.getName xs
    let title = T.intercalate " - " <$> zs
    let d = ffor len (maybe "" $ \TaskLength{..} ->
                showTaskDistance taskRoute)

    elClass "div" "tile" $ do
        elClass "div" "tile is-parent" $ do
            elClass "div" "tile is-child box" $ do
                elClass "p" "title is-3" $ do
                    dynText title
                elClass "p" "subtitle is-6" $ do
                    dynText d

taskDetail
    :: MonadWidget t m
    => IxTask
    -> Dynamic t [Comp]
    -> Dynamic t Task
    -> Dynamic t (Maybe Validity)
    -> Dynamic t (Maybe Allocation)
    -> m (Event t IxTask)

taskDetail ix@(IxTask _) cs task vy a = do
    let utc = utcOffset . head <$> cs
    s <- getTaskScore ix
    vw <- getTaskValidityWorking ix
    nyp <- getTaskPilotNyp ix
    dnf <- getTaskPilotDnf ix
    let lnTask = getTaskLengthSphericalEdge ix
    ln <- (fmap . fmap) taskLength lnTask
    legs <- (fmap . fmap) taskLegs lnTask
    routes <- getTaskLengthSphericalEdge ix

    let ps = (fmap . fmap) points a
    let tp = (fmap . fmap) taskPoints a
    let wg = (fmap . fmap) weight a

    taskTileZones task ln
    es <- simpleList cs (crumbTask task)
    tab <- tabsTask

    _ <- widgetHold (tableTask utc task legs) $
            (\case
                TaskTabTask -> tableTask utc task legs

                TaskTabMap -> mdo
                    p <- viewMap ix task routes pt
                    p' <- holdDyn nullPilot p
                    t <- getTaskPilotTrack ix p
                    let pt = attachPromptlyDyn p' t

                    return ()

                TaskTabAbsent -> tableAbsent nyp dnf task
                TaskTabValidity -> viewValidity vy vw
                TaskTabScore -> tableScore utc ln dnf vy wg ps tp s)
            <$> tab

    return $ switchDyn (leftmost <$> es)

taskDetail IxTaskNone _ _ _ _ = return never
