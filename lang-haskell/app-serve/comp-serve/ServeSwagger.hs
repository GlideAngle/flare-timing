{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ServeSwagger (SwagUiApi) where

import Data.Ratio
import Data.UnitsOfMeasure (KnownUnit, Unpack)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import GHC.Generics (Generic)
import Servant (Proxy(..))
import Data.Swagger
import Servant.Swagger.UI
import Control.Lens

import Flight.Zone.ZoneKind
import Flight.Zone.MkZones (Discipline(..), Zones(..))
import Flight.Zone.TaskZones (TaskZones(..))
import Flight.Zone.Raw
import Flight.Zone
import Flight.LatLng
import Flight.LatLng.Raw
import Flight.Geodesy
import Flight.Earth.Ellipsoid
import Flight.Score
import Flight.Comp

type SwagUiApi = SwaggerSchemaUI "swagger-ui" "swagger.json"

deriving instance Generic (LwScaling)
deriving instance Generic (Radius q)
deriving instance Generic (Alt q)
deriving instance Generic (RawLat)
deriving instance Generic (RawLng)
deriving instance Generic (ScoreBackTime k)
deriving instance Generic (EarthMath)
deriving instance Generic (Projection)
deriving instance Generic (NominalDistance q)
deriving instance Generic (MinimumDistance q)
deriving instance Generic (NominalTime q)
deriving instance Generic (NominalLaunch)
deriving instance Generic (NominalGoal)
deriving instance Generic (SecondsPerPoint k)
deriving instance Generic (TooEarlyPoints)
deriving instance Generic (JumpTheGunLimit k)

instance ToSchema (Ratio Integer) where
    declareNamedSchema _ = NamedSchema Nothing <$> declareSchema (Proxy :: Proxy Double)

instance (KnownUnit (Unpack u)) => ToSchema (Quantity Double u) where
    declareNamedSchema _ = NamedSchema Nothing <$> declareSchema (Proxy :: Proxy String)

instance ToSchema (TaskZones OpenDistance Double) where
    declareNamedSchema _ = do
        doubleSchema <- declareSchemaRef (Proxy :: Proxy Double)
        return . NamedSchema (Just "OpenDistance") $ mempty
          & (type_ .~ SwaggerObject)
          & properties .~
              [ ("prolog", doubleSchema)
              , ("open-mandatory", doubleSchema)
              , ("open-free", doubleSchema)
              ]
          & required .~ ["prolog", "open-mandatory", "open-free"]

instance ToSchema (TaskZones Race Double) where
    declareNamedSchema _ = do
        doubleSchema <- declareSchemaRef (Proxy :: Proxy Double)
        return . NamedSchema (Just "Race") $ mempty
          & (type_ .~ SwaggerObject)
          & properties .~
              [ ("prolog", doubleSchema)
              , ("race", doubleSchema)

              -- TzEssIsNotGoal fields
              , ("race-ess", doubleSchema)
              , ("epilog", doubleSchema)
              , ("goal", doubleSchema)

              -- TzEssIsGoal fields
              , ("race-ess-is-goal", doubleSchema)
              ]
          & required .~ ["prolog", "race"]

instance ToSchema (Zones)
instance ToSchema (RawZone)
instance ToSchema q => ToSchema (Alt q)
instance ToSchema (RawLat)
instance ToSchema (RawLng)
instance ToSchema (EarthMath)
instance ToSchema (Projection)
instance ToSchema (Ellipsoid Double)
instance ToSchema (EarthModel Double)
instance ToSchema (Discipline)
instance ToSchema (UtcOffset)
instance ToSchema (Give)
instance ToSchema (LwScaling)
instance ToSchema k => ToSchema (JumpTheGunLimit k)
instance ToSchema (PointPenalty)
instance ToSchema (TooEarlyPoints)
instance ToSchema (EarlyStart)
instance ToSchema (OpenClose)
instance ToSchema (StartGate)
instance ToSchema (TaskStop)
instance ToSchema k => ToSchema (ScoreBackTime k)
instance ToSchema (Tweak)
instance ToSchema (PilotId)
instance ToSchema (PilotName)
instance ToSchema (Pilot)
instance ToSchema q => ToSchema (Radius q)
instance ToSchema q => ToSchema (SecondsPerPoint q)
instance ToSchema (Task k)
instance ToSchema (Comp)
instance ToSchema (Nominal)
instance ToSchema q => ToSchema (NominalDistance q)
instance ToSchema q => ToSchema (MinimumDistance q)
instance ToSchema q => ToSchema (NominalTime q)
instance ToSchema (NominalLaunch)
instance ToSchema (NominalGoal)
