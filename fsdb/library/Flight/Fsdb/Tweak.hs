{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.Fsdb.Tweak (parseTweak) where

import Text.XML.HXT.Arrow.Pickle
    ( PU(..)
    , unpickleDoc', xpWrap, xpFilterAttr
    , xpElem, xpAttr
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
            False -> Just $ LwScaling 1
            True -> Nothing
        , \Tweak{..} ->
            maybe
                True
                (\case
                    LwScaling 1 -> False
                    _ -> True)
                leadingWeightScaling
        )
    $ (xpAttr "double_leading_weight" xpBool)

getTweak :: ArrowXml a => a XmlTree (Either String Tweak)
getTweak =
    getChildren
    >>> deep (hasName "FsCompetition")
    /> hasName "FsScoreFormula"
    >>> arr (unpickleDoc' xpTweak)

parseTweak :: String -> IO (Either String [Tweak])
parseTweak contents = do
    let doc = readString [ withValidate no, withWarnings no ] contents
    xs <- runX $ doc >>> getTweak
    return $ sequence xs
