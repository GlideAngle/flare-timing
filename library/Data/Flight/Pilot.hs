module Data.Flight.Pilot (Pilot(..), parse) where

import Text.XML.HXT.DOM.TypeDefs (XmlTree)
import Text.XML.HXT.Core
    ( ArrowXml
    , (>>>)
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

newtype Pilot = Pilot String deriving Show

getPilot :: ArrowXml a => a XmlTree Pilot
getPilot =
    getChildren
    >>> deep (hasName "FsCompetition")
    /> hasName "FsParticipants"
    /> hasName "FsParticipant"
    >>> getAttrValue "name"
    >>> arr Pilot

parse :: String -> IO (Either String [ Pilot ])
parse contents = do
    let doc = readString [ withValidate no, withWarnings no ] contents
    xs <- runX $ doc >>> getPilot
    return $ Right xs
