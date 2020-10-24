module Flight.Fsdb.Stopped (parseScoreBack) where

import Text.XML.HXT.DOM.TypeDefs (XmlTree)
import Text.XML.HXT.Arrow.Pickle
    ( PU(..)
    , xpWrap, xpAttr, xpPrim, xpFilterAttr, xpTrees, unpickleDoc'
    , xpPair, xpElem, xpOption
    )
import Text.XML.HXT.Core
    ( ArrowXml
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
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import "flight-gap-stop" Flight.Score (ScoreBackTime(..))

xpScoreBack :: PU (Maybe (ScoreBackTime (Quantity Double [u| s |])))
xpScoreBack =
    xpElem "FsScoreFormula"
    $ xpFilterAttr (hasName "score_back_time")
    $ xpWrap
        ( fmap (ScoreBackTime . MkQuantity . (* 60.0)) . fst
        , (, []) . fmap (\(ScoreBackTime (MkQuantity x)) -> x / 60)
        )
    $ xpPair
        (xpOption $ xpAttr "score_back_time" xpPrim)
        xpTrees

getScoreBack
    :: ArrowXml a
    => a XmlTree (Either String (Maybe (ScoreBackTime (Quantity Double [u| s |]))))
getScoreBack =
    getChildren
    >>> deep (hasName "FsCompetition")
    >>> getScoreFormula
    where
        getScoreFormula =
            getChildren
            >>> hasName "FsScoreFormula"
            >>> arr (unpickleDoc' xpScoreBack)

parseScoreBack
    :: String
    -> IO (Either String [Maybe (ScoreBackTime (Quantity Double [u| s |]))])
parseScoreBack contents = do
    let doc = readString [ withValidate no, withWarnings no ] contents
    xs <- runX $ doc >>> getScoreBack
    return $ sequence xs
