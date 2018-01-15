{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Flight.Fsdb.Nominal (parseNominal, pFloat) where

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
    , option
    , choice
    )
import qualified Text.ParserCombinators.Parsec as P (parse)
import Text.Parsec.Language (emptyDef)
import Data.Functor.Identity (Identity)
import Text.Parsec.Prim (ParsecT, parsecMap)
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Comp (Nominal(..))
import Flight.Score
    ( NominalLaunch(..)
    , NominalGoal(..)
    , NominalDistance(..)
    , MinimumDistance(..)
    , NominalTime(..)
    )

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
            >>> getAttrValue "nom_launch"
            &&& getAttrValue "nom_goal"
            &&& getAttrValue "nom_dist"
            &&& getAttrValue "min_dist"
            &&& getAttrValue "nom_time"
            >>> arr (\(l, (g, (d, (m, t)))) -> either (const []) id $ do
                l' <-
                    NominalLaunch .toRational
                    <$> P.parse pNominalLaunch "" l

                g' <-
                    NominalGoal . toRational
                    <$> P.parse pNominalGoal "" g

                d' <-
                    NominalDistance . MkQuantity
                    <$> P.parse pNominalDistance "" d

                m' <-
                    MinimumDistance . MkQuantity
                    <$> P.parse pMinimumDistance "" m

                t' :: Quantity Double [u| h |] <-
                    MkQuantity <$> P.parse pNominalTime "" t

                return [Nominal l' g' d' m' (NominalTime t')])


pNominalTime :: GenParser Char st Double
pNominalTime =
    choice
        [ pFloat <?> "No nominal time as float"
        , fromIntegral <$> pNat <?> "No nominal time as a nat"
        ]

pNominalLaunch :: GenParser Char st Double
pNominalLaunch =
    option 1
    $ choice
        [ pFloat <?> "No nominal launch as float"
        , fromIntegral <$> pNat <?> "No nominal launch as a nat"
        ]

pNominalGoal :: GenParser Char st Double
pNominalGoal =
    choice
        [ pFloat <?> "No nominal goal as float"
        , fromIntegral <$> pNat <?> "No nominal goal as a nat"
        ]

pNominalDistance :: GenParser Char st Double
pNominalDistance = pFloat <?> "No nominal distance"

pMinimumDistance :: GenParser Char st Double
pMinimumDistance = pFloat <?> "No minimum distance"

parseNominal :: String -> IO (Either String [Nominal])
parseNominal contents = do
    let doc = readString [ withValidate no, withWarnings no ] contents
    xs <- runX $ doc >>> getNominal
    return . Right . concat $ xs
