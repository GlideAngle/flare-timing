{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.Fsdb.Nominal (parseNominal) where

import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Text.XML.HXT.Arrow.Pickle
    ( XmlPickler(), PU(..)
    , xpickle, unpickleDoc', xpWrap, xp6Tuple, xpFilterAttr, xpDefault
    , xpElem, xpTrees, xpAttr
    )
import Text.XML.HXT.DOM.TypeDefs (XmlTree)
import Text.XML.HXT.Core
    ( ArrowXml
    , (<+>)
    , (>>>)
    , (/>)
    , runX
    , withValidate
    , withWarnings
    , readString
    , no
    , hasName
    , getChildren
    , deep
    , arr
    )

import Flight.Comp (Nominal(..), defaultNominal)
import "flight-gap-allot" Flight.Score
    ( NominalLaunch(..)
    , NominalGoal(..)
    , NominalDistance(..)
    , MinimumDistance(..)
    , NominalTime(..)
    )
import Flight.Fsdb.Internal.XmlPickle (xpNewtypeRational, xpNewtypeQuantity)

instance XmlPickler NominalLaunch where
    xpickle = xpNewtypeRational

instance XmlPickler NominalGoal where
    xpickle = xpNewtypeRational

instance (u ~ Quantity Double [u| km |]) => XmlPickler (NominalDistance u) where
    xpickle = xpNewtypeQuantity

instance (u ~ Quantity Double [u| km |]) => XmlPickler (MinimumDistance u) where
    xpickle = xpNewtypeQuantity

instance (u ~ Quantity Double [u| h |]) => XmlPickler (NominalTime u) where
    xpickle = xpNewtypeQuantity

xpNominal :: PU Nominal
xpNominal =
    xpElem "FsScoreFormula"
    $ xpFilterAttr
        ( hasName "nom_launch"
        <+> hasName "nom_goal"
        <+> hasName "nom_dist"
        <+> hasName "min_dist"
        <+> hasName "nom_time"
        )
    $ xpWrap
        ( \(nl, ng, nd, md, nt, _) -> Nominal nl ng nd md nt
        , \Nominal{..} -> (launch, goal, distance, free, time, [])
        )
    $ xp6Tuple
        (xpDefault (launch defaultNominal) $ xpAttr "nom_launch" xpickle)
        (xpDefault (goal defaultNominal) $ xpAttr "nom_goal" xpickle)
        (xpDefault (distance defaultNominal) $ xpAttr "nom_dist" xpickle)
        (xpDefault (free defaultNominal) $ xpAttr "min_dist" xpickle)
        (xpDefault (time defaultNominal) $ xpAttr "nom_time" xpickle)
        xpTrees

getNominal :: ArrowXml a => a XmlTree (Either String Nominal)
getNominal =
    getChildren
    >>> deep (hasName "FsCompetition")
    /> hasName "FsScoreFormula"
    >>> arr (unpickleDoc' xpNominal)

parseNominal :: String -> IO (Either String [Nominal])
parseNominal contents = do
    let doc = readString [ withValidate no, withWarnings no ] contents
    xs <- runX $ doc >>> getNominal
    return $ sequence xs
