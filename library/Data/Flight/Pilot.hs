{-# LANGUAGE ScopedTypeVariables #-}

module Data.Flight.Pilot (Pilot(..), parse) where

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

newtype Pilot = Pilot String
newtype KeyPilot = KeyPilot (String, String) deriving Show
newtype Key = Key String deriving Show
newtype TaskKey = TaskKey (String, [ Key ]) deriving Show

instance Show Pilot where
    show (Pilot name) = name

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

parse :: String -> IO (Either String [[ Pilot ]])
parse contents = do
    let doc = readString [ withValidate no, withWarnings no ] contents
    xs :: [ KeyPilot ] <- runX $ doc >>> getCompPilot
    ys :: [ TaskKey ] <- runX $ doc >>> getTaskPilot

    let xs' :: [ String ] =
            (\(KeyPilot (_, name)) -> name) <$> xs

    let compPilots :: [ Pilot ] = Pilot <$> xs'

    let xsMap :: Map String String =
            fromList $ (\(KeyPilot x) -> x) <$> xs

    let zs :: [[ String ]] =
            (\(TaskKey (_, ks)) -> (\(Key y) ->
                findWithDefault y y xsMap) <$> ks)
            <$> ys

    let taskPilots :: [[ Pilot ]] = (fmap . fmap) Pilot zs

    return $ Right $ compPilots : taskPilots
