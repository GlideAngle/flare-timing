module Flight.Fsdb.TaskEffort (parseNormEfforts) where

import Data.UnitsOfMeasure (u)
import Text.XML.HXT.Arrow.Pickle
    ( PU(..)
    , xpFilterAttr, xpFilterCont
    , xpInt, unpickleDoc', xpWrap, xpElem, xpAttr, xpPair
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
    , isAttr
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
    -- WARNING: Filter only attributes, ignoring child elements such as
    -- <FsChunk ... />. If not then the pickling will fail with
    -- "xpCheckEmptyContents: unprocessed XML content detected".
    $ xpFilterCont(isAttr)
    $ xpFilterAttr (hasName "look_ahead" <+> hasName "no_of_pilots_lo")
    $ xpWrap
        ( \(ahead, lo) ->
            nullLanding
                { lookahead = Just $ Lookahead ahead
                , landout = lo
                }
        , \TaskLanding{..} ->
            ( maybe 0 (\(Lookahead ahead) -> ahead) lookahead
            , landout
            )
        )
    $ xpPair
        (xpAttr "look_ahead" xpInt)
        (xpAttr "no_of_pilots_lo" xpInt)

getEffort :: ArrowXml a => a XmlTree (Either String TaskLanding)
getEffort =
    getChildren
    >>> deep (hasName "FsTaskDifficulty")
    >>> arr (unpickleDoc' xpDifficulty)


parseNormEfforts :: String -> IO (Either String [TaskLanding])
parseNormEfforts contents = do
    let doc = readString [ withValidate no, withWarnings no ] contents
    xs <- runX $ doc >>> getEffort
    return $ sequence xs
