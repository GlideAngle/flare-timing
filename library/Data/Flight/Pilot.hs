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
    )

newtype Pilot = Pilot String
newtype KeyPilot = KeyPilot (String, String) deriving Show
newtype Key = Key String deriving Show

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

getTaskPilot :: ArrowXml a => a XmlTree Key
getTaskPilot =
    getChildren
    >>> deep (hasName "FsTask")
    /> hasName "FsParticipants"
    /> hasName "FsParticipant"
    >>> getAttrValue "id"
    >>> arr Key

parse :: String -> IO (Either String [[ Pilot ]])
parse contents = do
    let doc = readString [ withValidate no, withWarnings no ] contents
    xs <- runX $ doc >>> getCompPilot
    ys <- runX $ doc >>> getTaskPilot

    let compPilots :: [ String ] = (\(KeyPilot (_, name)) -> name) <$> xs
    let xsMap :: Map String String = fromList $ (\(KeyPilot x) -> x) <$> xs
    let taskPilots :: [ String ] = (\(Key y) -> findWithDefault y y xsMap) <$> ys
    return $ Right $ [ Pilot <$> compPilots, Pilot <$> taskPilots ]
