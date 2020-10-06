{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.Fsdb.Tweak (parseTweak, xpTweak) where

import Data.Maybe (fromMaybe)
import Control.Arrow ((***))
import Control.Monad (join)
import Text.XML.HXT.Arrow.Pickle
    ( PU(..)
    , unpickleDoc', xpWrap, xpElem, xpAttr, xpTextAttr, xpOption, xpFilterAttr, xp6Tuple
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
import "flight-gap-weight" Flight.Score (LwScaling(..))
import Flight.Track.Lead (lwScalingDefault)
import Flight.Fsdb.Internal.XmlPickle (xpBool)

-- | The default is to use the square of leading area distance since 2016.
leadingSquareAreaDefault :: String -> Bool
leadingSquareAreaDefault formula = not $ formula `elem` xs where
    xs =
        [ "OzGAP2005"
        , "GAP2000"
        , "GAP2002"
        , "GAP2007"
        , "GAP2008"
        , "GAP2009"
        , "GAP2011"
        , "GAP2012"
        , "GAP2013"
        , "GAP2014"
        , "GAP2015"
        ]

xpTweak :: Discipline -> PU Tweak
xpTweak discipline =
    xpElem "FsScoreFormula"
    $ xpFilterAttr
        (hasName "id"
        <+> hasName "use_leading_points"
        <+> hasName "use_distance_squared_for_LC"
        <+> hasName "double_leading_weight"
        <+> hasName "use_arrival_position_points"
        <+> hasName "use_arrival_time_points"
        )
    $ xpWrap
        ( \(formula, lp, lsq, dlw, ar, at) ->
            let ls =
                    case (lp, dlw) of
                        (Just False, _) -> Just $ LwScaling 0
                        (Just True, Just False) -> Just $ LwScaling 1
                        (Just True, Just True) -> Nothing
                        (Just True, Nothing) -> Nothing
                        (Nothing, _) -> Nothing

                ls' = if Just (lwScalingDefault discipline) == ls then Nothing else ls

            in
                Tweak
                    { leadingWeightScaling = ls'
                    , leadingAreaDistanceSquared = fromMaybe (leadingSquareAreaDefault formula) lsq
                    , arrivalRank = ar && discipline /= Paragliding
                    , arrivalTime = at && discipline /= Paragliding
                    }
        , \Tweak
            { leadingWeightScaling = ls
            , leadingAreaDistanceSquared = lsq
            , arrivalRank = ar
            , arrivalTime = at
            } ->
            let (lp, lw) =
                    -- SEE: https://stackoverflow.com/questions/9722689/haskell-how-to-map-a-tuple
                    join (***) Just $
                    case ls of
                        Just (LwScaling 0) -> (False, True)
                        Just (LwScaling 1) -> (True, False)
                        Just (LwScaling _) -> (True, True)
                        Nothing -> (True, True)

            in ("", lp, Just lsq, lw, ar, at)
        )
    $ xp6Tuple
        (xpTextAttr "id")
        (xpOption $ xpAttr "use_leading_points" xpBool)
        (xpOption $ xpAttr "use_distance_squared_for_LC" xpBool)
        (xpOption $ xpAttr "double_leading_weight" xpBool)
        (xpAttr "use_arrival_position_points" xpBool)
        (xpAttr "use_arrival_time_points" xpBool)

getCompTweak
    :: ArrowXml a
    => Discipline
    -> a XmlTree (Either String Tweak)
getCompTweak discipline =
    (getChildren
    >>> deep (hasName "FsCompetition")
    /> hasName "FsScoreFormula"
    >>> arr (unpickleDoc' $ xpTweak discipline))
    `orElse` (constA . Right $ Tweak Nothing False (discipline == HangGliding) False)

parseTweak :: Discipline -> String -> IO (Either String [Tweak])
parseTweak discipline contents = do
    let doc = readString [ withValidate no, withWarnings no ] contents
    xs <- runX $ doc >>> getCompTweak discipline
    return $ sequence xs
