{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.Fsdb.Tweak (parseTweak) where

import Text.XML.HXT.Arrow.Pickle
    ( PU(..)
    , unpickleDoc', xpWrap, xpElem, xpAttr, xpOption, xpFilterAttr
    )
import Text.XML.HXT.DOM.TypeDefs (XmlTree)
import Text.XML.HXT.Core
    ( ArrowXml
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

import Flight.Comp (Tweak(..))
import Flight.Score (LwScaling(..))
import Flight.Fsdb.Internal.XmlPickle (xpBool)

xpTweak :: PU Tweak
xpTweak =
    xpElem "FsScoreFormula"
    $ xpFilterAttr (hasName "double_leading_weight")
    $ xpWrap
        ( Tweak . \case
            Just False -> Just $ LwScaling 1
            Just True -> Nothing
            Nothing -> Nothing
        , \Tweak{..} ->
            maybe
                (Just True)
                (\case
                    LwScaling 1 -> Just False
                    _ -> Just True)
                leadingWeightScaling
        )
    $ (xpOption $ xpAttr "double_leading_weight" xpBool)

getTweak :: ArrowXml a => a XmlTree (Either String Tweak)
getTweak =
    (getChildren
    >>> deep (hasName "FsCompetition")
    /> hasName "FsScoreFormula"
    >>> arr (unpickleDoc' xpTweak))
    `orElse` (constA . Right $ Tweak Nothing)

parseTweak :: String -> IO (Either String [Tweak])
parseTweak contents = do
    let doc = readString [ withValidate no, withWarnings no ] contents
    xs <- runX $ doc >>> getTweak
    return $ sequence xs
