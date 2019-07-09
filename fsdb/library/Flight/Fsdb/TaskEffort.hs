module Flight.Fsdb.TaskEffort (parseNormEfforts) where

import Data.UnitsOfMeasure (u)
import Text.XML.HXT.Arrow.Pickle
    ( PU(..)
    , xpInt, xpOption
    , unpickleDoc, xpWrap, xpFilterAttr, xpElem, xpAttr, xpPair
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
    , arr
    , deep
    )

import Flight.Units ()
import Flight.Fsdb.Internal.XmlPickle ()
import Flight.Track.Land (TaskLanding(..))
import Flight.Score (MinimumDistance(..), Lookahead(..))

nullLanding :: TaskLanding
nullLanding =
    TaskLanding
        { minDistance = MinimumDistance [u| 0 km |]
        , bestDistance = Nothing
        , landout = 0
        , lookahead = Nothing
        , chunking = Nothing
        , difficulty = Nothing
        }

xpDifficulty :: PU TaskLanding
xpDifficulty =
    xpElem "FsTaskDifficulty"
    $ xpFilterAttr (hasName "lookahead" <+> hasName "no_of_pilots_lo")
    $ xpWrap
        ( \(ahead, lo) ->
            nullLanding
                { lookahead = Lookahead <$> ahead
                , landout = lo
                }
        , \TaskLanding{..} ->
            ( (\(Lookahead ahead) -> ahead) <$> lookahead
            , landout
            )
        )
    $ xpPair
        (xpOption $ xpAttr "lookahead" xpInt)
        (xpAttr "no_of_pilots_lo" xpInt)

getEffort :: ArrowXml a => a XmlTree (Maybe TaskLanding)
getEffort =
    getChildren
    >>> deep (hasName "FsTask")
    >>> getDifficulty
    where
        getDifficulty =
            getChildren
            >>> deep (hasName "FsTaskDifficulty")
            >>> arr (unpickleDoc xpDifficulty)


parseNormEfforts :: String -> IO (Either String [Maybe TaskLanding])
parseNormEfforts contents = do
    let doc = readString [ withValidate no, withWarnings no ] contents
    xs <- runX $ doc >>> getEffort
    return $ Right xs
