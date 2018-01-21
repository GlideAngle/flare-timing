{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}

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
    , optionMaybe
    , choice
    )
import qualified Text.ParserCombinators.Parsec as P (parse)
import Text.Parsec.Language (emptyDef)
import Data.Functor.Identity (Identity)
import Text.Parsec.Prim (ParsecT, parsecMap)
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

getNominal :: ArrowXml a => Nominal -> a XmlTree [Nominal]
getNominal Nominal{launch = nl, goal = ng, distance = nd, free = nf, time = nt} =
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
            >>> arr (\(l, (g, (d, (m, t)))) ->
                return $
                Nominal
                    { launch =
                        either
                            (const nl)
                            (maybe nl (NominalLaunch . toRational))
                            (P.parse pNominalLaunch "" l)
                    , goal =
                        either
                            (const ng)
                            (maybe ng (NominalGoal . toRational))
                            (P.parse pNominalGoal "" g)
                    , distance =
                        either
                            (const nd)
                            (maybe nd (NominalDistance . MkQuantity))
                            (P.parse pNominalDistance "" d)
                    , free =
                        either
                            (const nf)
                            (maybe nf (MinimumDistance . MkQuantity))
                            (P.parse pMinimumDistance "" m)
                    , time =
                        either
                            (const nt)
                            (maybe nt (NominalTime . MkQuantity))
                            (P.parse pNominalTime "" t)
                    })

pNominalTime :: GenParser Char st (Maybe Double)
pNominalTime =
    optionMaybe
    $ choice
        [ pFloat <?> "No nominal time as float"
        , fromIntegral <$> pNat <?> "No nominal time as a nat"
        ]

pNominalLaunch :: GenParser Char st (Maybe Double)
pNominalLaunch =
    optionMaybe
    $ choice
        [ pFloat <?> "No nominal launch as float"
        , fromIntegral <$> pNat <?> "No nominal launch as a nat"
        ]

pNominalGoal :: GenParser Char st (Maybe Double)
pNominalGoal =
    optionMaybe
    $ choice
        [ pFloat <?> "No nominal goal as float"
        , fromIntegral <$> pNat <?> "No nominal goal as a nat"
        ]

pNominalDistance :: GenParser Char st (Maybe Double)
pNominalDistance =
    optionMaybe (pFloat <?> "No nominal distance")

pMinimumDistance :: GenParser Char st (Maybe Double)
pMinimumDistance =
    optionMaybe (pFloat <?> "No minimum distance")

parseNominal :: Nominal -> String -> IO (Either String [Nominal])
parseNominal defNominal contents = do
    let doc = readString [ withValidate no, withWarnings no ] contents
    xs <- runX $ doc >>> getNominal defNominal
    return . Right . concat $ xs
