module Flight.Fsdb.KeyPilot
    ( KeyPilot(..)
    , keyPilots
    , keyMap
    , unKeyPilot
    ) where

import Data.Map.Strict (Map, fromList, findWithDefault)

import Flight.Comp (PilotId(..), PilotName(..), Pilot(..))

newtype KeyPilot = KeyPilot (PilotId, Pilot)

keyPilots :: Functor f => f Pilot -> f KeyPilot
keyPilots ps = (\x@(Pilot (k, _)) -> KeyPilot (k, x)) <$> ps

keyMap :: [KeyPilot] -> Map PilotId Pilot
keyMap = fromList . fmap (\(KeyPilot x) -> x)

unKeyPilot :: Map PilotId Pilot -> PilotId -> Pilot
unKeyPilot ps k@(PilotId ip) =
    findWithDefault (Pilot (k, PilotName ip)) k ps
