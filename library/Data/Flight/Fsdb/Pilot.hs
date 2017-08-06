{-# LANGUAGE ScopedTypeVariables #-}

module Data.Flight.Fsdb.Pilot
    ( parsePilots
    , parseTracks
    , parseTaskFolders
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

import Data.Flight.Comp
    (TaskFolder(..), Pilot(..), TrackLogFile(..), PilotTrackLogFile(..))

newtype KeyPilot = KeyPilot (String, String) deriving Show
newtype KeyTrackLogFile = KeyTrackLogFile (String, String) deriving Show
newtype Key = Key String deriving Show
newtype TaskKey = TaskKey (String, [ Key ]) deriving Show

newtype TaskKeyTrackLogFile =
    TaskKeyTrackLogFile (String, [ KeyTrackLogFile ]) deriving Show

getCompPilot :: ArrowXml a => a XmlTree KeyPilot
getCompPilot =
    getChildren
    >>> deep (hasName "FsCompetition")
    /> hasName "FsParticipants"
    /> hasName "FsParticipant"
    >>> getAttrValue "id"
    &&& getAttrValue "name"
    >>> arr KeyPilot

getTaskFolder :: ArrowXml a => a XmlTree TaskFolder
getTaskFolder =
    getChildren
    >>> deep (hasName "FsTask")
    >>> getAttrValue "tracklog_folder"
    >>> arr (splitOneOf "\\/")
    >>> arr TaskFolder

getTaskPilot :: ArrowXml a => a XmlTree TaskKey
getTaskPilot =
    getChildren
    >>> deep (hasName "FsTask")
    >>> getAttrValue "id"
    &&& getPilots
    >>> arr TaskKey
    where
        getPilots =
            getChildren
            >>> hasName "FsParticipants"
            >>> listA getPilot

        getPilot =
            getChildren
            >>> hasName "FsParticipant"
            >>> getAttrValue "id"
            >>> arr Key

getTaskPilotTrackLogFile :: ArrowXml a => a XmlTree TaskKeyTrackLogFile
getTaskPilotTrackLogFile =
    getChildren
    >>> deep (hasName "FsTask")
    >>> getAttrValue "id"
    &&& getPilots
    >>> arr TaskKeyTrackLogFile
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
            >>> arr KeyTrackLogFile

        getTrackLog =
            getChildren
            >>> hasName "FsFlightData"
            >>> getAttrValue "tracklog_filename"

parsePilots :: String -> IO (Either String [[ Pilot ]])
parsePilots contents = do
    let doc = readString [ withValidate no, withWarnings no ] contents
    xs :: [ KeyPilot ] <- runX $ doc >>> getCompPilot
    ys :: [ TaskKey ] <- runX $ doc >>> getTaskPilot

    let xs' :: [ String ] =
            sort $ (\(KeyPilot (_, name)) -> name) <$> xs

    let compPilots :: [ Pilot ] = Pilot <$> xs'

    let xsMap :: Map String String =
            fromList $ (\(KeyPilot x) -> x) <$> xs

    let zs :: [[ String ]] =
            (\(TaskKey (_, ks)) ->
                sort
                $ (\(Key y) -> findWithDefault y y xsMap) <$> ks)
            <$> ys

    let taskPilots :: [[ Pilot ]] = (fmap . fmap) Pilot zs

    return $ Right $ compPilots : taskPilots

parseTracks :: String -> IO (Either String [[ PilotTrackLogFile ]])
parseTracks contents = do
    let doc = readString [ withValidate no, withWarnings no ] contents
    xs :: [ KeyPilot ] <- runX $ doc >>> getCompPilot
    ys :: [ TaskKeyTrackLogFile ] <- runX $ doc >>> getTaskPilotTrackLogFile

    let xsMap :: Map String String =
            fromList $ (\(KeyPilot x) -> x) <$> xs

    let taskPilotLogs :: [[ PilotTrackLogFile ]] =
            (\(TaskKeyTrackLogFile (_, ks)) ->
                sort
                $ (\(KeyTrackLogFile (k, filename)) ->
                    let pilot =
                            findWithDefault k k xsMap

                        tlf =
                            if null filename
                                then Nothing
                                else Just (TrackLogFile filename)

                    in PilotTrackLogFile (Pilot pilot) tlf) <$> ks)
            <$> ys

    return $ Right taskPilotLogs

parseTaskFolders :: String -> IO (Either String [ TaskFolder ])
parseTaskFolders contents = do
    let doc = readString [ withValidate no, withWarnings no ] contents
    xs <- runX $ doc >>> getTaskFolder
    return $ Right xs
