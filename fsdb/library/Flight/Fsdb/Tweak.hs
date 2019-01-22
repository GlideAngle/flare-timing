{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.Fsdb.Tweak (parseTweak) where

import Control.Arrow ((***))
import Control.Monad (join)
import Text.XML.HXT.Arrow.Pickle
    ( PU(..)
    , unpickleDoc', xpWrap, xpElem, xpAttr, xpOption, xpFilterAttr, xpTriple
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
    , constA
    , orElse
    )

import Flight.Zone.MkZones (Discipline(..))
import Flight.Comp (Tweak(..))
import Flight.Score (LwScaling(..), AwScaling(..))
import Flight.Fsdb.Internal.XmlPickle (xpBool)

xpTweak :: Discipline -> PU Tweak
xpTweak discipline =
    xpElem "FsScoreFormula"
    $ xpFilterAttr
        (hasName "use_leading_points"
        <+> hasName "double_leading_weight"
        <+> hasName "use_arrival_position_points"
        )
    $ xpWrap
        ( \(lp, dlw, ap) ->
            let ls =
                    case (lp, dlw) of
                        (Just False, _) -> Just $ LwScaling 0
                        (Just True, Just False) -> Just $ LwScaling 1
                        (Just True, Just True) -> Nothing
                        (Just True, Nothing) -> Nothing
                        (Nothing, _) -> Nothing
                as =
                    if discipline == Paragliding then Nothing else
                    case ap of
                        Just False -> Just $ AwScaling 0
                        Just True -> Nothing
                        Nothing -> Nothing
            in
                Tweak
                    { leadingWeightScaling = ls
                    , arrivalWeightScaling = as
                    }
        , \Tweak{leadingWeightScaling = ls, arrivalWeightScaling = as} ->
            let (lp, lw) =
                    -- SEE: https://stackoverflow.com/questions/9722689/haskell-how-to-map-a-tuple
                    join (***) Just $
                    case ls of
                        Just (LwScaling 0) -> (False, True)
                        Just (LwScaling 1) -> (True, False)
                        Just (LwScaling _) -> (True, True)
                        Nothing -> (True, True)
                ap =
                    Just $
                    case as of
                        Just (AwScaling 0) -> False
                        Just (AwScaling _) -> True
                        Nothing -> True
                  

            in (lp, lw, ap)
        )
    $ xpTriple
        (xpOption $ xpAttr "use_leading_points" xpBool)
        (xpOption $ xpAttr "double_leading_weight" xpBool)
        (xpOption $ xpAttr "use_arrival_position_points" xpBool)

getTweak
    :: ArrowXml a
    => Discipline
    -> a XmlTree (Either String Tweak)
getTweak discipline =
    (getChildren
    >>> deep (hasName "FsCompetition")
    /> hasName "FsScoreFormula"
    >>> arr (unpickleDoc' $ xpTweak discipline))
    `orElse` (constA . Right $ Tweak Nothing Nothing)

parseTweak :: Discipline -> String -> IO (Either String [Tweak])
parseTweak discipline contents = do
    let doc = readString [ withValidate no, withWarnings no ] contents
    xs <- runX $ doc >>> getTweak discipline
    return $ sequence xs
