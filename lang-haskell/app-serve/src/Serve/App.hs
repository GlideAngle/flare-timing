module Serve.App
    ( apiVersion
    , compInputSwagUiApi
    , taskLengthSwagUiApi
    , gapPointSwagUiApi
    , compInputSwagDoc
    , taskLengthSwagDoc
    , gapPointSwagDoc
    , serverCompInputSwagUiApi
    , serverTaskLengthSwagUiApi
    , serverGapPointSwagUiApi
    , serverCompInputApi
    , serverTaskLengthApi
    , serverGapPointApi
    , convertApp
    , mkCompInputApp
    , mkTaskLengthApp
    , mkGapPointApp
    ) where

import qualified Data.Text as T (Text)
import Network.Wai (Application)
import Network.Wai.Middleware.Cors (simpleCors)
import Servant
    ( (:<|>)(..)
    , Server, Handler(..), Proxy(..)
    , hoistServer, serve
    )
import Data.Swagger (Swagger(..), URL(..), url, info, title, version, license, description)
import Servant.Swagger
import Servant.Swagger.UI
import Control.Lens hiding (ix)
import Control.Monad.Reader (asks, runReaderT)

import Flight.Units ()
import qualified Flight.Track.Point as Alt (AltPointing(..))
import Flight.Track.Point (Pointing(..))
import Flight.Comp (AltDot(AltFs, AltAs), CompSettings(..))
import ServeSwagger (SwagUiApi)
import Serve.Config (AppT(..), Config(..))
import Serve.Api
    ( CompInputApi, TaskLengthApi, GapPointApi
    , compInputApi, taskLengthApi, gapPointApi
    )
import Serve.PointDiff (getTaskPointsDiffStats)
import Serve.Route
    ( getTaskRouteSphericalEdge
    , getTaskRouteEllipsoidEdge
    , getTaskRouteProjectedSphericalEdge
    , getTaskRouteProjectedEllipsoidEdge
    , getTaskRouteProjectedPlanarEdge
    , getTaskRouteLengths
    )
import Serve.Pilot
    ( getTaskPilotAbs
    , getTaskPilotDnf
    , getTaskPilotDfNoTrack
    , getTaskPilotNyp
    , getTaskPilotDf
    , getTaskPilotTrack
    , getTaskPilotArea
    , getTaskPilotTrackFlyingSection
    , getTaskPilotTrackScoredSection
    , getTaskPilotCross
    , getTaskPilotTag
    , getPilots
    , getPilotsStatus
    )
import Serve.Task
    ( getTaskScore
    , getTaskValidityWorking
    , getTaskFlyingSectionTimes
    , getTaskBolsterStats
    , getTaskBonusBolsterStats
    , getTaskReach
    , getTaskBonusReach
    , getTaskEffort
    , getTaskLanding
    , getTaskArrival
    , getTaskLead
    , getTaskTime
    , getValidity
    , getAllocation
    )
import Serve.Alt
    ( getAltTaskScore
    , getAltTaskValidityWorking
    , getAltTaskRouteSphere
    , getAltTaskRouteEllipse
    , getAltTaskLanding
    , getAltTaskArrival
    )

type CompInputSwagUiApi k = SwagUiApi :<|> CompInputApi k
type TaskLengthSwagUiApi k = SwagUiApi :<|> TaskLengthApi k
type GapPointSwagUiApi k = SwagUiApi :<|> GapPointApi k

compInputSwagUiApi :: Proxy (CompInputSwagUiApi k)
compInputSwagUiApi = Proxy

taskLengthSwagUiApi :: Proxy (TaskLengthSwagUiApi k)
taskLengthSwagUiApi = Proxy

gapPointSwagUiApi :: Proxy (GapPointSwagUiApi k)
gapPointSwagUiApi = Proxy

apiVersion :: T.Text
apiVersion = "0.25"

compInputSwagDoc :: Swagger
compInputSwagDoc = toSwagger compInputApi
  & info.title   .~ "Comp Input API"
  & info.version .~ apiVersion
  & info.description ?~ "The subset of endpoints served when only comp inputs are available."
  & info.license ?~ ("MPL" & url ?~ URL "http://mozilla.org/MPL/2.0/")

taskLengthSwagDoc :: Swagger
taskLengthSwagDoc = toSwagger taskLengthApi
  & info.title   .~ "Task Length API"
  & info.version .~ apiVersion
  & info.description ?~ "The subset of endpoints served when only comp inputs and task lengths are available."
  & info.license ?~ ("MPL" & url ?~ URL "http://mozilla.org/MPL/2.0/")

gapPointSwagDoc :: Swagger
gapPointSwagDoc = toSwagger gapPointApi
  & info.title   .~ "Gap Point API"
  & info.version .~ apiVersion
  & info.description ?~ "The full set of endpoints served when the comp has been scored."
  & info.license ?~ ("MPL" & url ?~ URL "http://mozilla.org/MPL/2.0/")

convertApp :: Config k -> AppT k IO a -> Handler a
convertApp cfg appt = Handler $ runReaderT (unApp appt) cfg

-- SEE: https://stackoverflow.com/questions/42143155/acess-a-servant-server-with-a-reflex-dom-client
mkCompInputApp :: Config k -> IO Application
mkCompInputApp cfg = return . simpleCors . serve compInputSwagUiApi $ serverCompInputSwagUiApi cfg

mkTaskLengthApp :: Config k -> IO Application
mkTaskLengthApp cfg = return . simpleCors . serve taskLengthSwagUiApi $ serverTaskLengthSwagUiApi cfg

mkGapPointApp :: Config k -> IO Application
mkGapPointApp cfg = return . simpleCors . serve gapPointSwagUiApi $ serverGapPointSwagUiApi cfg

serverCompInputSwagUiApi :: Config k -> Server (CompInputSwagUiApi k)
serverCompInputSwagUiApi cfg =
    swaggerSchemaUIServer compInputSwagDoc :<|> serverCompInputApi cfg

serverTaskLengthSwagUiApi :: Config k -> Server (TaskLengthSwagUiApi k)
serverTaskLengthSwagUiApi cfg =
    swaggerSchemaUIServer taskLengthSwagDoc :<|> serverTaskLengthApi cfg

serverGapPointSwagUiApi :: Config k -> Server (GapPointSwagUiApi k)
serverGapPointSwagUiApi cfg =
    swaggerSchemaUIServer gapPointSwagDoc :<|> serverGapPointApi cfg

serverCompInputApi :: Config k -> Server (CompInputApi k)
serverCompInputApi cfg =
    hoistServer compInputApi (convertApp cfg) $
        comp <$> c
        :<|> nominal <$> c
        :<|> tasks <$> c
        :<|> getPilots <$> c
    where
        c = asks compSettings

serverTaskLengthApi :: Config k -> Server (TaskLengthApi k)
serverTaskLengthApi cfg =
    hoistServer taskLengthApi (convertApp cfg) $
        comp <$> c
        :<|> nominal <$> c
        :<|> tasks <$> c
        :<|> getPilots <$> c
        :<|> getTaskRouteSphericalEdge
        :<|> getTaskRouteEllipsoidEdge
        :<|> getTaskRouteProjectedSphericalEdge
        :<|> getTaskRouteProjectedEllipsoidEdge
        :<|> getTaskRouteProjectedPlanarEdge
        :<|> getTaskRouteLengths
    where
        c = asks compSettings

serverGapPointApi :: Config k -> Server (GapPointApi k)
serverGapPointApi cfg =
    hoistServer gapPointApi (convertApp cfg) $
        comp <$> c
        :<|> nominal <$> c
        :<|> tasks <$> c
        :<|> getPilots <$> c
        :<|> getTaskRouteSphericalEdge
        :<|> getTaskRouteEllipsoidEdge
        :<|> getTaskRouteProjectedSphericalEdge
        :<|> getTaskRouteProjectedEllipsoidEdge
        :<|> getTaskRouteProjectedPlanarEdge
        :<|> getTaskRouteLengths
        :<|> getTaskPointsDiffStats
        :<|> getPilotsStatus
        :<|> getValidity <$> p
        :<|> getAllocation <$> p
        :<|> getTaskScore
        :<|> getTaskValidityWorking
        :<|> getTaskPilotAbs
        :<|> getTaskPilotDnf
        :<|> getTaskPilotDfNoTrack
        :<|> getTaskPilotNyp
        :<|> getTaskPilotDf
        :<|> getTaskPilotTrack
        :<|> getTaskPilotArea
        :<|> getTaskPilotTrackFlyingSection
        :<|> getTaskPilotTrackScoredSection
        :<|> getTaskFlyingSectionTimes
        :<|> getTaskPilotCross
        :<|> getTaskPilotTag
        :<|> getTaskBolsterStats
        :<|> getTaskBonusBolsterStats
        :<|> getTaskReach
        :<|> getTaskBonusReach
        :<|> getTaskArrival
        :<|> getTaskLead
        :<|> getTaskTime
        :<|> getTaskEffort
        :<|> getTaskLanding

        :<|> getAltTaskLanding
        :<|> getAltTaskRouteSphere
        :<|> getAltTaskRouteEllipse
        :<|> getValidity <$> n
        :<|> getAltTaskValidityWorking
        :<|> getAltTaskScore AltFs
        :<|> getAltTaskArrival
        :<|> getAltTaskScore AltAs
    where
        c = asks compSettings
        p = asks pointing
        n = asks altFsScore
