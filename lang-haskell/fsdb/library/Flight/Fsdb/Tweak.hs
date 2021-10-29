{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.Fsdb.Tweak (parseTweak, xpTweak) where

import Data.Maybe (fromMaybe)
import Control.Arrow ((***))
import Control.Monad (join)
import Text.XML.HXT.Arrow.Pickle
    ( PU(..)
    , unpickleDoc', xpPrim
    , xpWrap, xpElem, xpAttr, xpTextAttr, xpOption, xpDefault, xpFilterAttr, xp8Tuple
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
import "flight-gap-allot" Flight.Score (powerExp23, powerExp56)
import "flight-gap-weight" Flight.Score (LwScaling(..), EGwScaling(..))
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
        <+> hasName "use_flat_decline_of_timepoints"
        <+> hasName "time_points_if_not_in_goal"
        )
    $ xpWrap
        ( \(formula, lp, lsq, dlw, ar, at, tpe, eg :: Maybe Double) ->
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
                    , timePowerExponent = if tpe then powerExp56 else powerExp23
                    , essNotGoalScaling = EGwScaling $ maybe 0 toRational eg
                    }
        , \Tweak
            { leadingWeightScaling = ls
            , leadingAreaDistanceSquared = lsq
            , arrivalRank = ar
            , arrivalTime = at
            , timePowerExponent
            , essNotGoalScaling = EGwScaling eg
            } ->
            let (lp, lw) =
                    -- SEE: https://stackoverflow.com/questions/9722689/haskell-how-to-map-a-tuple
                    join (***) Just $
                    case ls of
                        Just (LwScaling 0) -> (False, True)
                        Just (LwScaling 1) -> (True, False)
                        Just (LwScaling _) -> (True, True)
                        Nothing -> (True, True)

                tpe = timePowerExponent == powerExp56

            in ("", lp, Just lsq, lw, ar, at, tpe, Just $ fromRational eg)
        )
    $ xp8Tuple
        (xpTextAttr "id")
        (xpOption $ xpAttr "use_leading_points" xpBool)
        (xpOption $ xpAttr "use_distance_squared_for_LC" xpBool)
        (xpOption $ xpAttr "double_leading_weight" xpBool)
        (xpAttr "use_arrival_position_points" xpBool)
        (xpAttr "use_arrival_time_points" xpBool)
        (xpDefault False $ xpAttr "use_flat_decline_of_timepoints" xpBool)
        (xpOption $ xpAttr "time_points_if_not_in_goal" xpPrim)

getCompTweak
    :: ArrowXml a
    => Discipline
    -> a XmlTree (Either String Tweak)
getCompTweak discipline =
    (getChildren
    >>> deep (hasName "FsCompetition")
    /> hasName "FsScoreFormula"
    >>> arr (unpickleDoc' $ xpTweak discipline))
    `orElse`
    (constA . Right $
        Tweak Nothing False (discipline == HangGliding) False powerExp23 (EGwScaling 0))

parseTweak :: Discipline -> String -> IO (Either String [Tweak])
parseTweak discipline contents = do
    let doc = readString [ withValidate no, withWarnings no ] contents
    xs <- runX $ doc >>> getCompTweak discipline
    return $ sequence xs
