module Flight.Fsdb.Nominal (parseNominal, pFloat) where

import Debug.Trace
import Data.Either (either)
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
import Text.Parsec.Token as P
import Text.ParserCombinators.Parsec
    ( GenParser
    , (<?>)
    , (<|>)
    )
import qualified Text.ParserCombinators.Parsec as P (parse)
import Text.Parsec.Language (emptyDef)
import Data.Functor.Identity (Identity)
import Text.Parsec.Prim (ParsecT, parsecMap)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Comp (Nominal(..))
import Flight.Score (NominalGoal(..), NominalDistance(..))

lexer :: GenTokenParser String u Identity
lexer = P.makeTokenParser emptyDef

pFloat:: ParsecT String u Identity Double
pFloat = parsecMap (either fromInteger id) $ P.naturalOrFloat lexer 

pNat :: ParsecT String u Identity Integer
pNat = P.natural lexer 

getNominal :: ArrowXml a => a XmlTree [Nominal]
getNominal =
    getChildren
    >>> deep (hasName "FsCompetition")
    >>> getScoreFormula
    where
        getScoreFormula =
            getChildren
            >>> hasName "FsScoreFormula"
            >>> getAttrValue "nom_dist"
            &&& getAttrValue "min_dist"
            &&& getAttrValue "nom_time"
            &&& getAttrValue "nom_goal"
            >>> arr (\(d, (m, (t, g))) -> either (const []) id $ do
                d' <-
                    NominalDistance . MkQuantity
                    <$> P.parse pNominalDistance "" d

                m' <- P.parse pMinimumDistance "" m

                g' <-
                    NominalGoal . toRational
                    <$> P.parse pNominalGoal "" g

                return [Nominal d' m' t g'])


pNominalGoal :: GenParser Char st Double
pNominalGoal =
    (pFloat <?> "No nominal goal as float")
    <|> fromIntegral <$> (pNat <?> "No nominal goal as a nat")

pNominalDistance :: GenParser Char st Double
pNominalDistance = pFloat <?> "No nominal distance"

pMinimumDistance :: GenParser Char st Double
pMinimumDistance = pFloat <?> "No minimum distance"

parseNominal :: String -> IO (Either String [Nominal])
parseNominal contents = do
    let doc = readString [ withValidate no, withWarnings no ] contents
    xs <- runX $ doc >>> getNominal
    return . Right . concat $ traceShow xs xs
