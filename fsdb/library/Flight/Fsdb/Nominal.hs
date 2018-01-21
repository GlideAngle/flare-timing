{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}

module Flight.Fsdb.Nominal (parseNominal) where

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
import Control.Applicative
import Text.Megaparsec
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Data.Void
import Data.Functor.Identity
import Data.Scientific

import Flight.Comp (Nominal(..))
import Flight.Score
    ( NominalLaunch(..)
    , NominalGoal(..)
    , NominalDistance(..)
    , MinimumDistance(..)
    , NominalTime(..)
    )
import Flight.Fsdb.Internal

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
                            (maybe nl (NominalLaunch . toRational . sciToFloat))
                            (prs pNominalLaunch l)
                    , goal =
                        either
                            (const ng)
                            (maybe ng (NominalGoal . toRational . sciToFloat))
                            (prs pNominalGoal g)
                    , distance =
                        either
                            (const nd)
                            (maybe nd (NominalDistance . MkQuantity . sciToFloat))
                            (prs pNominalDistance d)
                    , free =
                        either
                            (const nf)
                            (maybe nf (MinimumDistance . MkQuantity . sciToFloat))
                            (prs pMinimumDistance m)
                    , time =
                        either
                            (const nt)
                            (maybe nt (NominalTime . MkQuantity . sciToFloat))
                            (prs pNominalTime t)
                    })

pNominalTime :: ParsecT Void String Identity (Maybe Scientific)
pNominalTime =
    optional (sci <?> "No nominal time as float")

pNominalLaunch :: ParsecT Void String Identity (Maybe Scientific)
pNominalLaunch =
    optional (sci <?> "No nominal launch as float")

pNominalGoal :: ParsecT Void String Identity (Maybe Scientific)
pNominalGoal =
    optional (sci <?> "No nominal goal as float")

pNominalDistance :: ParsecT Void String Identity (Maybe Scientific)
pNominalDistance =
    optional (sci <?> "No nominal distance")

pMinimumDistance :: ParsecT Void String Identity (Maybe Scientific)
pMinimumDistance =
    optional (sci <?> "No minimum distance")

parseNominal :: Nominal -> String -> IO (Either String [Nominal])
parseNominal defNominal contents = do
    let doc = readString [ withValidate no, withWarnings no ] contents
    xs <- runX $ doc >>> getNominal defNominal
    return . Right . concat $ xs
