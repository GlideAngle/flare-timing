module Flight.Fsdb.Pilot
    ( parsePilots
    , parseTracks
    , parseTaskFolders
    , getCompPilot
    ) where

import Data.List (sort)
import Data.List.Split (splitOneOf)
import Data.Map.Strict (Map, fromList, findWithDefault)
import Text.XML.HXT.DOM.TypeDefs (XmlTree)
import Text.XML.HXT.Core
    ( ArrowXml
    , (>>>)
    , (&&&)
    , (/>)
    , runX
    , withValidate
    , withWarnings
    , readString
    , no
    , hasName
    , getChildren
    , getAttrValue
    , deep
    , arr
    , listA
    )

import Flight.Comp
    ( PilotId(..), PilotName(..), Pilot(..)
    , TaskFolder(..), Pilot(..), TrackLogFile(..), PilotTrackLogFile(..)
    )

newtype TaskId = TaskId String deriving Show

newtype KeyTrackLogFile = KeyTrackLogFile (PilotId, String) deriving Show
newtype TaskPilots = TaskPilots (TaskId, [PilotId]) deriving Show

newtype TaskKeyTrackLogFile =
    TaskKeyTrackLogFile (TaskId, [KeyTrackLogFile]) deriving Show

getCompPilot :: ArrowXml a => a XmlTree Pilot
getCompPilot =
    getChildren
    >>> deep (hasName "FsCompetition")
    /> hasName "FsParticipants"
    /> hasName "FsParticipant"
    >>> getAttrValue "id"
    &&& getAttrValue "name"
    >>> arr (\(k, p) -> Pilot (PilotId k, PilotName p))

getTaskFolder :: ArrowXml a => a XmlTree TaskFolder
getTaskFolder =
    getChildren
    >>> deep (hasName "FsTask")
    >>> getAttrValue "tracklog_folder"
    >>> arr (splitOneOf "\\/")
    >>> arr TaskFolder

getTaskPilot :: ArrowXml a => a XmlTree TaskPilots
getTaskPilot =
    getChildren
    >>> deep (hasName "FsTask")
    >>> getAttrValue "id"
    &&& getPilots
    >>> arr (\(k, ps) -> TaskPilots (TaskId k, ps))
    where
        getPilots =
            getChildren
            >>> hasName "FsParticipants"
            >>> listA getPilot

        getPilot =
            getChildren
            >>> hasName "FsParticipant"
            >>> getAttrValue "id"
            >>> arr PilotId

getTaskPilotTrackLogFile :: ArrowXml a => a XmlTree TaskKeyTrackLogFile
getTaskPilotTrackLogFile =
    getChildren
    >>> deep (hasName "FsTask")
    >>> getAttrValue "id"
    &&& getPilots
    >>> arr (\(k, xs) -> TaskKeyTrackLogFile (TaskId k, xs))
    where
        getPilots =
            getChildren
            >>> hasName "FsParticipants"
            >>> listA getPilot

        getPilot =
            getChildren
            >>> hasName "FsParticipant"
            >>> getAttrValue "id"
            &&& getTrackLog
            >>> arr (\(k, x) -> KeyTrackLogFile (PilotId k, x))

        getTrackLog =
            getChildren
            >>> hasName "FsFlightData"
            >>> getAttrValue "tracklog_filename"

parsePilots :: String -> IO (Either String [[Pilot]])
parsePilots contents = do
    let doc = readString [withValidate no, withWarnings no] contents
    xs :: [Pilot] <- runX $ doc >>> getCompPilot
    taskPilots :: [TaskPilots] <- runX $ doc >>> getTaskPilot

    let xsMap :: Map PilotId Pilot =
            fromList $ (\x@(Pilot (k, _)) -> (k, x)) <$> xs

    let ys :: [[Pilot]] =
            (\(TaskPilots (_, ks)) ->
                sort
                $ (\k@(PilotId s) ->
                    findWithDefault (Pilot (k, PilotName s)) k xsMap)
                    <$> ks)
            <$> taskPilots

    return $ Right $ xs : ys

parseTracks :: String -> IO (Either String [[PilotTrackLogFile]])
parseTracks contents = do
    let doc = readString [withValidate no, withWarnings no] contents
    xs :: [Pilot] <- runX $ doc >>> getCompPilot
    ys :: [TaskKeyTrackLogFile] <- runX $ doc >>> getTaskPilotTrackLogFile

    let xsMap :: Map PilotId Pilot =
            fromList $ (\x@(Pilot (k, _)) -> (k, x)) <$> xs

    let taskPilotLogs :: [[PilotTrackLogFile]] =
            (\(TaskKeyTrackLogFile (_, ks)) ->
                sort
                $ (\(KeyTrackLogFile (k@(PilotId s), filename)) ->
                    let pilot =
                            findWithDefault (Pilot (k, PilotName s)) k xsMap

                        tlf =
                            if null filename
                                then Nothing
                                else Just (TrackLogFile filename)

                    in PilotTrackLogFile pilot tlf) <$> ks)
            <$> ys

    return $ Right taskPilotLogs

parseTaskFolders :: String -> IO (Either String [TaskFolder])
parseTaskFolders contents = do
    let doc = readString [withValidate no, withWarnings no] contents
    xs <- runX $ doc >>> getTaskFolder
    return $ Right xs
