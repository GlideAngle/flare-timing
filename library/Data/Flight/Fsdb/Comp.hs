module Data.Flight.Fsdb.Comp (parseComp) where

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

import Data.Flight.Comp (Comp(..))

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

parseComp :: String -> IO (Either String [ Comp ])
parseComp contents = do
    let doc = readString [ withValidate no, withWarnings no ] contents
    xs <- runX $ doc >>> getComp
    return $ Right xs
