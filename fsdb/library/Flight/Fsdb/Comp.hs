module Flight.Fsdb.Comp (parseComp) where

import Control.Newtype
import Text.XML.HXT.Arrow.Pickle
    ( XmlPickler(), PU(..)
    , xpickle, unpickleDoc', xpWrap, xp9Tuple, xpFilterAttr, xpDefault
    , xpElem, xpTrees, xpAttr, xpPrim, xpInt, xpOption, xpText
    )
import Text.XML.HXT.DOM.TypeDefs (XmlTree)
import Text.XML.HXT.Core
    ( ArrowXml
    , (<+>)
    , (>>>)
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

import Flight.Score (Discipline(..))
import Flight.Comp (Comp(..), UtcOffset(..))

-- | A newtype wrapper of Comp for pickling.
newtype XComp = XComp Comp
instance (Newtype XComp Comp) where
    pack = XComp
    unpack (XComp o) = o

instance XmlPickler XComp where
    xpickle = xpComp

xpComp :: PU XComp
xpComp =
    xpElem "FsCompetition"
    $ xpFilterAttr
        ( hasName "id"
        <+> hasName "name"
        <+> hasName "discipline"
        <+> hasName "location"
        <+> hasName "from"
        <+> hasName "to"
        <+> hasName "utc_offset"
        <+> hasName "score_back"
        )
    $ xpWrap
        ( \(i, n, d, l, f, t, utc, s, _) ->
            XComp (Comp i n d l f t (UtcOffset $ 60 * utc) s)
        , \(XComp Comp{..}) ->
            ( civilId
            , compName
            , discipline
            , location
            , from
            , to
            , let (UtcOffset utc) = utcOffset in utc `div` 60
            , scoreBack
            , []
            )
        )
    $ xp9Tuple
        (xpAttr "id" xpText)
        (xpAttr "name" xpText)
        (xpDefault HangGliding $ xpAttr "discipline" xpPrim)
        (xpAttr "location" xpText)
        (xpAttr "from" xpText)
        (xpAttr "to" xpText)
        (xpAttr "utc_offset" xpInt)
        (xpOption $ xpAttr "score_back" xpPrim)
        xpTrees

getComp :: ArrowXml a => a XmlTree (Either String Comp)
getComp =
    getChildren
    >>> deep (hasName "FsCompetition")
    >>> arr (unpickleDoc' xpComp)
    >>> arr (unpack <$>)

parseComp :: String -> IO (Either String [Comp])
parseComp contents = do
    let doc = readString [ withValidate no, withWarnings no ] contents
    xs <- runX $ doc >>> getComp
    return $ sequence xs
