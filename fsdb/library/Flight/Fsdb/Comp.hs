module Flight.Fsdb.Comp (parseComp) where

import Text.XML.HXT.Arrow.Pickle
    ( PU(..)
    , unpickleDoc', xpWrap, xp9Tuple, xpFilterAttr, xpDefault
    , xpElem, xpTrees, xpAttr, xpPrim, xpInt, xpOption, xpTextAttr
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

import Flight.Zone.MkZones (Discipline(..))
import Flight.Comp
    (Projection(..), EarthMath(..), EarthModel(..), Comp(..), UtcOffset(..))

xpComp :: PU Comp
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
            let e = EarthAsFlat UTM; em = Pythagorus in
            Comp
                { civilId = i
                , compName = n
                , discipline = d
                , location = l
                , from = f
                , to = t
                , utcOffset = UtcOffset $ 60 * utc
                , scoreBack = s
                , give = Nothing
                , earth = e
                , earthMath = em
                }
        , \Comp{..} ->
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
        (xpTextAttr "id")
        (xpTextAttr "name")
        (xpDefault HangGliding $ xpAttr "discipline" xpPrim)
        (xpTextAttr "location")
        (xpTextAttr "from")
        (xpTextAttr "to")
        (xpAttr "utc_offset" xpInt)
        (xpOption $ xpAttr "score_back" xpPrim)
        xpTrees

getComp :: ArrowXml a => a XmlTree (Either String Comp)
getComp =
    getChildren
    >>> deep (hasName "FsCompetition")
    >>> arr (unpickleDoc' xpComp)

parseComp :: String -> IO (Either String [Comp])
parseComp contents = do
    let doc = readString [ withValidate no, withWarnings no ] contents
    xs <- runX $ doc >>> getComp
    return $ sequence xs
