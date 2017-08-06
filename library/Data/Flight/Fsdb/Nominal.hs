module Data.Flight.Fsdb.Nominal (parseNominal) where

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

import Data.Flight.Comp (Nominal(..))

getNominal :: ArrowXml a => a XmlTree Nominal
getNominal =
    getChildren
    >>> deep (hasName "FsCompetition")
    >>> getScoreFormula
    where
        getScoreFormula =
            getChildren
            >>> hasName "FsScoreFormula"
            >>> getAttrValue "nom_dist"
            &&& getAttrValue "nom_time"
            &&& getAttrValue "nom_goal"
            >>> arr (\(d, (t, g)) -> Nominal d t g)

parseNominal :: String -> IO (Either String [Nominal])
parseNominal contents = do
    let doc = readString [ withValidate no, withWarnings no ] contents
    xs <- runX $ doc >>> getNominal
    return $ Right xs
