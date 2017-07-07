{-# LANGUAGE ScopedTypeVariables #-}

module Data.Flight.Pilot
    ( Pilot(..)
    , PilotTrackLogFile(..)
    , parseNames
    , parseTracks
    ) where

import Data.List (sort)
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

newtype Pilot = Pilot String deriving (Eq, Ord)
newtype TrackLogFile = TrackLogFile String deriving (Eq, Ord)

newtype KeyPilot = KeyPilot (String, String) deriving Show
newtype KeyTrackLogFile = KeyTrackLogFile (String, String) deriving Show
newtype Key = Key String deriving Show
newtype TaskKey = TaskKey (String, [ Key ]) deriving Show

data PilotTrackLogFile =
    PilotTrackLogFile Pilot (Maybe TrackLogFile) deriving (Eq, Ord)

newtype TaskKeyTrackLogFile =
    TaskKeyTrackLogFile (String, [ KeyTrackLogFile ]) deriving Show

instance Show Pilot where
    show (Pilot name) = name

instance Show TrackLogFile where
    show (TrackLogFile name) = name

instance Show PilotTrackLogFile where
    show (PilotTrackLogFile pilot Nothing) = show pilot ++ " -"
    show (PilotTrackLogFile pilot (Just tlf)) = show pilot ++ " <<" ++ show tlf ++ ">>"

getCompPilot :: ArrowXml a => a XmlTree KeyPilot
getCompPilot =
    getChildren
    >>> deep (hasName "FsCompetition")
    /> hasName "FsParticipants"
    /> hasName "FsParticipant"
    >>> getAttrValue "id"
    &&& getAttrValue "name"
    >>> arr KeyPilot

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

parseNames :: String -> IO (Either String [[ Pilot ]])
parseNames contents = do
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
