module FlareTiming.Comp.Pilot (tablePilot) where

import Reflex.Dom
import qualified Data.Text as T (pack)

import WireTypes.Comp (Task(..))
import WireTypes.Pilot (Pilot, PilotTaskStatus(..))
import FlareTiming.Pilot (showPilotId, showPilotName)
import FlareTiming.Comms (getPilotsStatus)

tablePilot
    :: MonadWidget t m
    => Dynamic t [Task]
    -> m ()
tablePilot ts = do
    pb <- getPostBuild
    xs <- holdDyn [] =<< getPilotsStatus pb
    _ <- elClass "table" "table is-bordered is-striped" $ do
            el "thead" $ do
                el "tr" $ do
                    el "th" $ text "Id"
                    el "th" $ text "Name"

                    _ <- simpleList ts thTask

                    return ()

            el "tbody" $ do
                simpleList xs (uncurry rowPilotStatus . splitDynPure)
    return ()

thTask
    :: MonadWidget t m
    => Dynamic t Task
    -> m ()
thTask t = do
    el "th" . dynText $ T.pack . taskName <$> t

rowPilotStatus
    :: MonadWidget t m
    => Dynamic t Pilot
    -> Dynamic t [PilotTaskStatus]
    -> m ()
rowPilotStatus p ts = do
    el "tr" $ do
        el "td" . dynText $ showPilotId <$> p
        el "td" . dynText $ showPilotName <$> p

        _ <- simpleList ts tdStatus

        return ()

tdStatus
    :: MonadWidget t m
    => Dynamic t PilotTaskStatus
    -> m ()
tdStatus s = do
    s' <- sample . current $ s
    el "td" . text $ if s' == DF then "" else T.pack . show $ s'

