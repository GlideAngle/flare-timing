module FlareTiming.Comms
    ( GetConstraint
    , getComps
    , getNominals
    , getTasks
    , getPilots
    , getPilotsStatus
    , getValidity
    , getAllocation
    , getTaskScore
    , getTaskValidityWorking
    , getTaskLengthSphericalEdge
    , getTaskPilotDnf
    , getTaskPilotNyp
    , getTaskPilotDf
    , getTaskPilotTrack
    ) where

import Reflex
import Reflex.Dom
import qualified Data.Text as T (Text, pack)
import Control.Monad.IO.Class (MonadIO)

import WireTypes.Comp (Comp(..), Nominal(..), Task(..))
import WireTypes.Route
import WireTypes.Pilot (PilotTaskStatus(..), Pilot(..), PilotId(..))
import WireTypes.Point
import WireTypes.Validity
import WireTypes.ValidityWorking
import FlareTiming.Events (IxTask(..))

-- NOTE: Possible alternatives for mapUri ...
-- mapUri s = "http://localhost:3000" <> s
-- mapUri s = "http://1976-never-land.flaretiming.com/json" <> s <> ".json"
-- mapUri s = "http://1989-lift-lines.flaretiming.com/json" <> s <> ".json"
-- mapUri s = "http://2012-forbes.flaretiming.com/json" <> s <> ".json"
-- mapUri s = "http://2018-forbes.flaretiming.com/json" <> s <> ".json"
-- mapUri s = "http://2018-dalmatian.flaretiming.com/json" <> s <> ".json"
-- mapUri s = "/json" <> s <> ".json"
mapUri :: T.Text -> T.Text
mapUri s = "http://localhost:3000" <> s

type GetConstraint t m =
    ( PostBuild t m
    , MonadIO (Performable m)
    , HasJSContext (Performable m)
    , PerformEvent t m
    , TriggerEvent t m
    , MonadHold t m
    )

getTasks :: GetConstraint t m => () -> m (Dynamic t [Task])
getTasks () = do
    pb :: Event t () <- getPostBuild
    let defReq = mapUri "/comp-input/tasks"
    let req md = XhrRequest "GET" (maybe defReq id md) def
    rsp <- performRequestAsync $ fmap req $ leftmost [ Nothing <$ pb ]

    let es :: Event t [Task] = fmapMaybe decodeXhrResponse rsp
    holdDyn [] es

getComps
    :: GetConstraint t m
    => ()
    -> m (Dynamic t [Comp])
getComps () = do
    pb :: Event t () <- getPostBuild
    let defReq = mapUri "/comp-input/comps"
    let req md = XhrRequest "GET" (maybe defReq id md) def
    rsp <- performRequestAsync $ fmap req $ leftmost [ Nothing <$ pb ]

    let es :: Event t Comp = fmapMaybe decodeXhrResponse rsp
    xs :: Dynamic t [Comp] <- holdDyn [] (pure <$> es)
    return xs

getNominals
    :: GetConstraint t m
    => ()
    -> m (Dynamic t [Nominal])
getNominals () = do
    pb :: Event t () <- getPostBuild
    let defReq = mapUri "/comp-input/nominals"
    let req md = XhrRequest "GET" (maybe defReq id md) def
    rsp <- performRequestAsync $ fmap req $ leftmost [ Nothing <$ pb ]

    let es :: Event t Nominal = fmapMaybe decodeXhrResponse rsp
    xs :: Dynamic t [Nominal] <- holdDyn [] (pure <$> es)
    return xs

getPilots
    :: GetConstraint t m
    => ()
    -> m (Dynamic t [Pilot])
getPilots () = do
    pb :: Event t () <- getPostBuild
    let defReq = mapUri "/comp-input/pilots"
    let req md = XhrRequest "GET" (maybe defReq id md) def
    rsp <- performRequestAsync $ fmap req $ leftmost [ Nothing <$ pb ]

    let es :: Event t [Pilot] = fmapMaybe decodeXhrResponse rsp
    holdDyn [] es

getPilotsStatus
    :: GetConstraint t m
    => ()
    -> m (Dynamic t [(Pilot, [PilotTaskStatus])])
getPilotsStatus () = do
    pb :: Event t () <- getPostBuild
    let defReq = mapUri "/gap-point/pilots-status"
    let req md = XhrRequest "GET" (maybe defReq id md) def
    rsp <- performRequestAsync $ fmap req $ leftmost [ Nothing <$ pb ]

    let es :: Event t [(Pilot, [PilotTaskStatus])] = fmapMaybe decodeXhrResponse rsp
    holdDyn [] es

getValidity
    :: GetConstraint t m
    => ()
    -> m (Dynamic t [Maybe Validity])
getValidity () = do
    pb :: Event t () <- getPostBuild
    let defReq = mapUri "/gap-point/validity"
    let req md = XhrRequest "GET" (maybe defReq id md) def
    rsp <- performRequestAsync $ fmap req $ leftmost [ Nothing <$ pb ]

    let es :: Event t [Maybe Validity] = fmapMaybe decodeXhrResponse rsp
    holdDyn [] es

getAllocation
    :: GetConstraint t m
    => ()
    -> m (Dynamic t [Maybe Allocation])
getAllocation () = do
    pb :: Event t () <- getPostBuild
    let defReq = mapUri "/gap-point/allocation"
    let req md = XhrRequest "GET" (maybe defReq id md) def
    rsp <- performRequestAsync $ fmap req $ leftmost [ Nothing <$ pb ]

    let es :: Event t [Maybe Allocation] = fmapMaybe decodeXhrResponse rsp
    holdDyn [] es

getTaskScore
    :: GetConstraint t m
    => IxTask
    -> m (Dynamic t [(Pilot, Breakdown)])

getTaskScore (IxTask ii) = do
    pb :: Event t () <- getPostBuild

    let defReq :: T.Text
        defReq =
            mapUri
            $ "/gap-point/"
            <> (T.pack . show $ ii)
            <> "/score"

    let req md = XhrRequest "GET" (maybe defReq id md) def
    rsp <- performRequestAsync $ fmap req $ leftmost [ Nothing <$ pb ]

    let es :: Event t [(Pilot, Breakdown)] = fmapMaybe decodeXhrResponse rsp
    holdDyn [] es

getTaskScore IxTaskNone = return $ constDyn []

getTaskValidityWorking
    :: GetConstraint t m
    => IxTask
    -> m (Dynamic t (Maybe ValidityWorking))
getTaskValidityWorking (IxTask ii) = do
    pb :: Event t () <- getPostBuild

    let defReq :: T.Text
        defReq =
            mapUri
            $ "/gap-point/"
            <> (T.pack . show $ ii)
            <> "/validity-working"

    let req md = XhrRequest "GET" (maybe defReq id md) def
    rsp <- performRequestAsync $ fmap req $ leftmost [ Nothing <$ pb ]

    let es :: Event t (Maybe ValidityWorking) = fmapMaybe decodeXhrResponse rsp
    holdDyn Nothing es

getTaskValidityWorking IxTaskNone = return $ constDyn Nothing

emptyRoute :: OptimalRoute (Maybe a)
emptyRoute =
    OptimalRoute
        { taskRoute = Nothing
        , taskRouteSpeedSubset = Nothing
        , speedRoute = Nothing
        }

getTaskLengthSphericalEdge
    :: GetConstraint t m
    => IxTask
    -> m (Dynamic t (OptimalRoute (Maybe TrackLine)))
getTaskLengthSphericalEdge (IxTask ii) = do
    pb :: Event t () <- getPostBuild

    let defReq :: T.Text
        defReq =
            mapUri
            $ "/task-length/"
            <> (T.pack . show $ ii)
            <> "/spherical-edge"

    let req md = XhrRequest "GET" (maybe defReq id md) def
    rsp <- performRequestAsync $ fmap req $ leftmost [ Nothing <$ pb ]

    let es :: Event t (OptimalRoute (Maybe TrackLine)) = fmapMaybe decodeXhrResponse rsp
    holdDyn emptyRoute es

getTaskLengthSphericalEdge IxTaskNone = return $ constDyn emptyRoute

getTaskPilot_
    :: GetConstraint t m
    => T.Text
    -> T.Text
    -> IxTask
    -> m (Dynamic t [Pilot])
getTaskPilot_ stage path (IxTask ii) = do
    pb :: Event t () <- getPostBuild

    let defReq :: T.Text
        defReq =
            mapUri
            $ "/"
            <> stage
            <> "/"
            <> (T.pack . show $ ii)
            <> "/"
            <> path


    let req md = XhrRequest "GET" (maybe defReq id md) def
    rsp <- performRequestAsync $ fmap req $ leftmost [ Nothing <$ pb ]

    let es :: Event t [Pilot] = fmapMaybe decodeXhrResponse rsp
    holdDyn [] es

getTaskPilot_ _ _ IxTaskNone = return $ constDyn []

getTaskPilotDnf, getTaskPilotNyp, getTaskPilotDf
    :: GetConstraint t m
    => IxTask
    -> m (Dynamic t [Pilot])
getTaskPilotDnf = getTaskPilot_ "cross-zone" "pilot-dnf"
getTaskPilotNyp = getTaskPilot_ "cross-zone" "pilot-nyp"
getTaskPilotDf = getTaskPilot_ "gap-point" "pilot-df"

getTaskPilotTrack
    :: GetConstraint t m
    => IxTask
    -> PilotId
    -> m (Dynamic t [[Double]])
getTaskPilotTrack (IxTask ii) (PilotId pid) = do
    pb :: Event t () <- getPostBuild

    let defReq :: T.Text
        defReq =
            mapUri
            $ "/pilot-track/"
            <> (T.pack . show $ ii)
            <> "/"
            <> (T.pack pid)

    let req md = XhrRequest "GET" (maybe defReq id md) def
    rsp <- performRequestAsync $ fmap req $ leftmost [ Nothing <$ pb ]

    let es :: Event t [[Double]] = fmapMaybe decodeXhrResponse rsp
    holdDyn [] es

getTaskPilotTrack IxTaskNone _ = return $ constDyn []
