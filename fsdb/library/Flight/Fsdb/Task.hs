{-# OPTIONS_GHC -fno-warn-orphans #-}

module Flight.Fsdb.Task (parseTasks) where

import Data.Maybe (catMaybes)
import Data.List (sort, nub)
import Data.Map.Strict (Map, fromList, findWithDefault)
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Text.XML.HXT.Arrow.Pickle
    ( XmlPickler(..), PU(..)
    , xpickle, unpickleDoc, xpWrap, xpFilterAttr, xpElem, xpAttr
    , xpText, xpPair, xpTriple, xp4Tuple, xpInt, xpTrees
    )
import Text.XML.HXT.DOM.TypeDefs (XmlTree)
import Text.XML.HXT.Core
    ( ArrowXml
    , (<+>)
    , (&&&)
    , (>>>)
    , (>>.)
    , runX
    , withValidate
    , withWarnings
    , readString
    , no
    , hasName
    , getChildren
    , getAttrValue
    , listA
    , arr
    , deep
    , notContaining
    )
import Data.Time.Clock (UTCTime)
import Data.Time.Format (parseTimeOrError, defaultTimeLocale)

import Flight.LatLng.Raw (RawLat(..), RawLng(..))
import Flight.Zone (Radius(..), Zone(..), RawZoneToZone, rawZonesToZones)
import qualified Flight.Zone.ZoneKind as ZK
    (ZoneKind(..), Goal, RawZoneToZoneKind, rawZonesToZoneKinds)
import qualified Flight.Zone.Raw as Z (RawZone(..))
import Flight.Comp
    ( PilotId(..), PilotName(..), Pilot(..)
    , Task(..), TaskStop(..), StartGate(..), OpenClose(..)
    )
import Flight.Fsdb.Pilot (getCompPilot)
import Flight.Units ()
import Flight.Score (Discipline(..))
import Flight.Fsdb.Internal.XmlPickle (xpNewtypeRational, xpNewtypeQuantity)

newtype KeyPilot = KeyPilot (PilotId, Pilot)

instance (u ~ Quantity Double [u| m |]) => XmlPickler (Radius u) where
    xpickle = xpNewtypeQuantity

keyMap :: [KeyPilot] -> Map PilotId Pilot
keyMap = fromList . fmap (\(KeyPilot x) -> x)
                        
unKeyPilot :: Map PilotId Pilot -> PilotId -> Pilot
unKeyPilot ps k@(PilotId ip) =
    findWithDefault (Pilot (k, PilotName ip)) k ps

instance XmlPickler RawLat where
    xpickle = xpNewtypeRational

instance XmlPickler RawLng where
    xpickle = xpNewtypeRational

xpZone :: PU Z.RawZone
xpZone =
    xpElem "FsTurnpoint"
    $ xpFilterAttr
        ( hasName "id"
        <+> hasName "lat"
        <+> hasName "lon"
        <+> hasName "radius"
        )
    $ xpWrap
        ( \(n, lat, lng, r) -> Z.RawZone n lat lng r
        , \(Z.RawZone{..}) -> (zoneName, lat, lng, radius)
        )
    $ xp4Tuple
        (xpAttr "id" xpText)
        (xpAttr "lat" xpickle)
        (xpAttr "lon" xpickle)
        (xpAttr "radius" xpickle)

xpOpenClose :: PU OpenClose
xpOpenClose =
    xpElem "FsTurnpoint"
    $ xpFilterAttr (hasName "open" <+> hasName "close")
    $ xpWrap
        ( \(open, close) -> OpenClose (parseUtcTime open) (parseUtcTime close)
        , \(OpenClose{..}) -> (show open, show close)
        )
    $ xpPair
        (xpAttr "open" xpText)
        (xpAttr "close" xpText)

xpStartGate :: PU StartGate
xpStartGate =
    xpElem "FsStartGate"
    $ xpFilterAttr (hasName "open")
    $ xpWrap
        ( StartGate . parseUtcTime
        , \(StartGate open) -> show open
        )
    $ xpAttr "open" xpText

xpSpeedSection :: PU (Int, Int)
xpSpeedSection =
    xpElem "FsTaskDefinition"
    $ xpFilterAttr (hasName "ss" <+> hasName "es")
    $ xpWrap
        ( (\(a, b, _) -> (a, b))
        , \(a, b) -> (a, b, [])
        )
    $ xpTriple
        (xpAttr "ss" xpInt)
        (xpAttr "es" xpInt)
        xpTrees

getTask :: ArrowXml a => Discipline -> [Pilot] -> a XmlTree (Task k)
getTask discipline ps =
    getChildren
    >>> deep (hasName "FsTask")
    >>> getAttrValue "name"
    &&& getStopped
    &&& getAbsent
    &&& getFormula
    &&& getDefn
    >>> arr mkTask
    where
        kps = (\x@(Pilot (k, _)) -> KeyPilot (k, x)) <$> ps
        
        getFormula =
            getChildren
            >>> hasName "FsScoreFormula"
            >>> getAttrValue "use_semi_circle_control_zone_for_goal_line"
            >>> arr (== "1")

        getDefn =
            getChildren
            >>> hasName "FsTaskDefinition"
            >>> (getSpeedSection &&& getGoal)
            >>. take 1
            &&& (listA getTps >>> arr catMaybes)
            &&& (listA getOpenClose >>> arr catMaybes)
            &&& (listA getGates >>> arr catMaybes)

        getSpeedSection =
            arr (unpickleDoc xpSpeedSection)

        getGoal =
            (getAttrValue "goal")

        getOpenClose =
            getChildren
            >>> hasName "FsTurnpoint"
            >>> arr (unpickleDoc xpOpenClose)

        getTps =
            getChildren
            >>> hasName "FsTurnpoint"
            >>> arr (unpickleDoc xpZone)

        getGates =
            getChildren
            >>> hasName "FsStartGate"
            >>> arr (unpickleDoc xpStartGate)

        getAbsent =
            getChildren
            >>> hasName "FsParticipants"
            >>> listA getAbsentees

        getAbsentees =
            getChildren
            >>> hasName "FsParticipant"
                `notContaining` (getChildren >>> hasName "FsFlightData")
            >>> getAttrValue "id"
            >>> arr (unKeyPilot (keyMap kps) . PilotId)

        getStopped =
            getChildren
            >>> hasName "FsTaskState"
            >>> getAttrValue "task_state"
            &&& getAttrValue "stop_time"
            >>> arr parseStop

        mkTask (name, (stop, (absentees, (useSemi, ((section, goal), (zs, (ts, gates))))))) =
            Task name zs zs' zs'' section ts'' gates (sort absentees) stop
            where
                -- NOTE: If all time zones are the same then collapse.
                ts' = nub ts
                ts'' = if length ts' == 1 then ts' else ts
                zs' = rawZonesToZones (mkGoal discipline useSemi goal) zs
                zs'' = ZK.rawZonesToZoneKinds (mkGoalKind discipline useSemi goal) zs

parseTasks :: Discipline -> String -> IO (Either String [Task k])
parseTasks discipline contents = do
    let doc = readString [ withValidate no, withWarnings no ] contents
    ps <- runX $ doc >>> getCompPilot
    xs <- runX $ doc >>> getTask discipline ps
    return $ Right xs

parseUtcTime :: String -> UTCTime
parseUtcTime =
    -- NOTE: %F is %Y-%m-%d, %T is %H:%M:%S and %z is -HHMM or -HH:MM
    parseTimeOrError False defaultTimeLocale "%FT%T%z"

parseStop :: (String, String) -> Maybe TaskStop
parseStop ("STOPPED", t) = Just . TaskStop $ parseUtcTime t
parseStop _ = Nothing

mkGoal :: Discipline -> Bool -> String -> RawZoneToZone
mkGoal Paragliding True "LINE" = SemiCircle
mkGoal _ _ "LINE" = Line
mkGoal _ _ _ = Circle

mkGoalKind :: Discipline -> Bool -> String -> ZK.RawZoneToZoneKind ZK.Goal
mkGoalKind Paragliding True "LINE" = ZK.SemiCircle
mkGoalKind _ _ "LINE" = ZK.Line
mkGoalKind _ _ _ = ZK.Circle
