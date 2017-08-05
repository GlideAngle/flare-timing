{-# LANGUAGE DeriveGeneric #-}

module Data.Flight.Comp (Comp(..), parse) where

import Text.XML.HXT.DOM.TypeDefs (XmlTree)
import Text.XML.HXT.Core
    ( ArrowXml
    , (&&&)
    , (>>>)
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
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))

data Comp = Comp { civilId :: String
                 , compName :: String 
                 , location :: String 
                 , from :: String 
                 , to :: String 
                 , utcOffset :: String 
                 } deriving (Show, Generic)

instance ToJSON Comp
instance FromJSON Comp

getComp :: ArrowXml a => a XmlTree Comp
getComp =
    getChildren
    >>> deep (hasName "FsCompetition")
    >>> getAttrValue "id"
    &&& getAttrValue "name"
    &&& getAttrValue "location"
    &&& getAttrValue "from"
    &&& getAttrValue "to"
    &&& getAttrValue "utc_offset"
    >>> arr (\(i, (n, (l, (f, (t, u))))) -> Comp i n l f t u)

parse :: String -> IO (Either String [ Comp ])
parse contents = do
    let doc = readString [ withValidate no, withWarnings no ] contents
    xs <- runX $ doc >>> getComp
    return $ Right xs
