module Flight.Fsdb.Task (parseTasks) where

import Data.List (sort, concatMap, nub)
import Data.Map.Strict (Map, fromList, findWithDefault)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Text.XML.HXT.DOM.TypeDefs (XmlTree)
import Text.XML.HXT.Core
    ( ArrowXml
    , (&&&)
    , (>>>)
    , (>>.)
    , (>.)
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
import Text.Megaparsec ((<?>))

import Flight.LatLng.Raw (RawLat(..), RawLng(..))
import Flight.Zone (Radius(..), Zone(..), RawZoneToZone, rawZonesToZones)
import qualified Flight.Zone.Raw as Z (RawZone(..))
import Flight.Comp
    ( PilotId(..), PilotName(..), Pilot(..)
    , Task(..), TaskStop(..), SpeedSection, StartGate(..), OpenClose(..)
    )
import Flight.Fsdb.Pilot (getCompPilot)
import Flight.Units ()
import Flight.Score (Discipline(..))
import Flight.Fsdb.Internal (prs, sci, sciToInt, sciToFloat, sciToRational)

newtype KeyPilot = KeyPilot (PilotId, Pilot)

keyMap :: [KeyPilot] -> Map PilotId Pilot
keyMap = fromList . fmap (\(KeyPilot x) -> x)
                        
unKeyPilot :: Map PilotId Pilot -> PilotId -> Pilot
unKeyPilot ps k@(PilotId ip) =
    findWithDefault (Pilot (k, PilotName ip)) k ps

getTask :: ArrowXml a => Discipline -> [Pilot] -> a XmlTree Task
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
            &&& listA getTps
            &&& listA getOpenClose
            &&& listA getGates

        getSpeedSection =
            (getAttrValue "ss" &&& getAttrValue "es")
            >. parseSpeedSection

        getGoal =
            (getAttrValue "goal")

        getOpenClose =
            getChildren
            >>> hasName "FsTurnpoint"
            >>> getAttrValue "open"
            &&& getAttrValue "close"
            >>> arr parseOpenClose

        getTps =
            getChildren
            >>> hasName "FsTurnpoint"
            >>> getAttrValue "id"
            &&& getAttrValue "lat"
            &&& getAttrValue "lon"
            &&& getAttrValue "radius"
            >>> arr (\(name, (lat', (lng', rad))) -> (name, lat', lng', rad))
            >>. concatMap parseZone

        getGates =
            getChildren
            >>> hasName "FsStartGate"
            >>> getAttrValue "open"
            >>> arr parseStartGate

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
            Task name zs zs' section ts'' gates (sort absentees) stop
            where
                -- NOTE: If all time zones are the same then collapse.
                ts' = nub ts
                ts'' = if length ts' == 1 then ts' else ts
                zs' = rawZonesToZones (mkGoal discipline useSemi goal) zs

parseTasks :: Discipline -> String -> IO (Either String [Task])
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

parseStartGate :: String -> StartGate
parseStartGate =
    StartGate . parseUtcTime

parseOpenClose :: (String, String) -> OpenClose
parseOpenClose (o, c) =
    OpenClose (parseUtcTime o) (parseUtcTime c)

parseSpeedSection :: [(String, String)] -> SpeedSection
parseSpeedSection [] = Nothing
parseSpeedSection ((ss, es) : _) =
    case section of
        Right [ ss', es' ] -> Just (fromInteger ss', fromInteger es')
        _ -> Nothing
    where
        section =
            sequence
                [ do
                    ss'' <- prs (sci <?> "Start of speed section") ss
                    return $ sciToInt ss''
                , do
                    es'' <- prs (sci <?> "End of speed section") es
                    return $ sciToInt es''
                ]

mkGoal :: Discipline -> Bool -> String -> RawZoneToZone
mkGoal Paragliding True "LINE" = SemiCircle
mkGoal _ _ "LINE" = Line
mkGoal _ _ _ = Circle

parseZone :: (String, String, String, String) -> [Z.RawZone]
parseZone (name, lat', lng', rad') = either (const []) (: []) $ do
    lat <- prs (sci <?> "No latitude") lat'
    lng <- prs (sci <?> "No longitude") lng'
    rad <- prs (sci <?> "No radius") rad'
    return $
        Z.RawZone
            name
            (RawLat $ sciToRational lat)
            (RawLng $ sciToRational lng)
            (Radius (MkQuantity $ sciToFloat rad))
