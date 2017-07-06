module Data.Flight.Nominal (Nominal(..), parse) where

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
    , unlistA
    , deep
    , arr
    )
import Text.Parsec.Token as P
import Text.ParserCombinators.Parsec
    ( GenParser
    , (<?>)
    , char
    , option
    )
import qualified Text.ParserCombinators.Parsec as P (parse)
import Text.Parsec.Language (emptyDef)
import Data.Functor.Identity (Identity)
import Text.Parsec.Prim (ParsecT, parsecMap)

data Nominal = Nominal { distance :: String
                       , time :: String 
                       , goal :: String 
                       } deriving Show

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

parse :: String -> IO (Either String [ Nominal ])
parse contents = do
    let doc = readString [ withValidate no, withWarnings no ] contents
    xs <- runX $ doc >>> getNominal
    return $ Right xs
