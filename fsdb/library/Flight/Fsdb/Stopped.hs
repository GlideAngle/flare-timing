module Flight.Fsdb.Stopped (parseStopped) where

import Text.XML.HXT.DOM.TypeDefs (XmlTree)
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
    , getAttrValue
    , deep
    , arr
    )
import Data.Bifunctor (bimap)
import Control.Applicative (optional)
import Text.Megaparsec ((<?>))
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.Score (ScoreBackTime(..))
import Flight.Fsdb.Internal (prs, sci, sciToInt)

getStopped
    :: ArrowXml a
    => a XmlTree (Either String (Maybe (ScoreBackTime (Quantity Double [u| s |]))))
getStopped =
    getChildren
    >>> deep (hasName "FsCompetition")
    >>> getScoreFormula
    where
        getScoreFormula =
            getChildren
            >>> hasName "FsScoreFormula"
            >>> getAttrValue "score_back_time"
            >>> arr parseScoreBack

parseScoreBack
    :: String -> Either String (Maybe (ScoreBackTime (Quantity Double [u| s |])))
parseScoreBack s =
    bimap show (fmap f) secs
    where
        f = ScoreBackTime . MkQuantity . fromIntegral . (* 60)
        mins = prs (optional (sci <?> "Seconds of score back time")) s
        secs = (fmap. fmap) sciToInt mins

parseStopped
    :: String
    -> IO (Either String [Maybe (ScoreBackTime (Quantity Double [u| s |]))])
parseStopped contents = do
    let doc = readString [ withValidate no, withWarnings no ] contents
    xs <- runX $ doc >>> getStopped
    return $ sequence xs
