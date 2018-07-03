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
import Control.Applicative (optional)
import Text.Megaparsec ((<?>))
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Comp (Nominal(..))
import Flight.Score
    ( NominalLaunch(..)
    , NominalGoal(..)
    , NominalDistance(..)
    , MinimumDistance(..)
    , NominalTime(..)
    )
import Flight.Fsdb.Internal (prs, sci, sciToFloat)

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
                            (prs (optional (sci <?> "No nominal launch as float")) l)
                    , goal =
                        either
                            (const ng)
                            (maybe ng (NominalGoal . toRational . sciToFloat))
                            (prs (optional (sci <?> "No nominal goal as float")) g)
                    , distance =
                        either
                            (const nd)
                            (maybe nd (NominalDistance . MkQuantity . sciToFloat))
                            (prs (optional (sci <?> "No nominal distance")) d)
                    , free =
                        either
                            (const nf)
                            (maybe nf (MinimumDistance . MkQuantity . sciToFloat))
                            (prs (optional (sci <?> "No minimum distance")) m)
                    , time =
                        either
                            (const nt)
                            (maybe nt (NominalTime . MkQuantity . sciToFloat))
                            (prs (optional (sci <?> "No nominal time as float")) t)
                    })

parseNominal :: Nominal -> String -> IO (Either String [Nominal])
parseNominal defNominal contents = do
    let doc = readString [ withValidate no, withWarnings no ] contents
    xs <- runX $ doc >>> getNominal defNominal
    return . Right . concat $ xs
