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

import Data.Bifunctor (bimap)
import Data.Flight.Comp (Comp(..), UtcOffset(..))
import Text.Parsec.Token as P
import qualified Text.ParserCombinators.Parsec as P (parse)
import Text.Parsec.Language (emptyDef)
import Data.Functor.Identity (Identity)
import Text.Parsec.Prim (ParsecT)

lexer :: GenTokenParser String u Identity
lexer = P.makeTokenParser emptyDef

pNat :: ParsecT String u Identity Integer
pNat = P.natural lexer 

getComp :: ArrowXml a => a XmlTree (Either String Comp)
getComp =
    getChildren
    >>> deep (hasName "FsCompetition")
    >>> getAttrValue "id"
    &&& getAttrValue "name"
    &&& getAttrValue "location"
    &&& getAttrValue "from"
    &&& getAttrValue "to"
    &&& getUtcOffset
    >>> arr (\(i, (n, (l, (f, (t, u))))) -> Comp i n l f t <$> u)
    where
        getUtcOffset =
            getAttrValue "utc_offset"
            >>> arr parseUtcOffset

parseUtcOffset :: String -> Either String UtcOffset
parseUtcOffset s =
    bimap show (UtcOffset . toMins) hrs
    where
        hrs = P.parse pNat "" s
        toMins h = fromIntegral (h * 60)

parseComp :: String -> IO (Either String [ Comp ])
parseComp contents = do
    let doc = readString [ withValidate no, withWarnings no ] contents
    xs <- runX $ doc >>> getComp
    return $ sequence xs
