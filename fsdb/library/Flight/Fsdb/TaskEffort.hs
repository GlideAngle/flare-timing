module Flight.Fsdb.TaskEffort (parseNormEfforts) where

import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Text.XML.HXT.Arrow.Pickle
    ( PU(..)
    , xpFilterAttr, xpFilterCont
    , xpInt, xpPrim, unpickleDoc', xpWrap, xpElem, xpAttr, xp5Tuple
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
import Flight.Score
    ( MinimumDistance(..), Lookahead(..), SumOfDifficulty(..)
    , IxChunk(..), Chunk(..), Chunking(..)
    )

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
    $ xpFilterAttr
        ( hasName "look_ahead"
        <+> hasName "start_chunks"
        <+> hasName "end_chunks"
        <+> hasName "sum_of_difficulty"
        <+> hasName "no_of_pilots_lo"
        )
    $ xpWrap
        ( \(ahead, sc, ec, sumDiff, lo) ->
            nullLanding
                { lookahead = Just $ Lookahead ahead
                , chunking =
                    Just $
                    Chunking
                        { sumOf = SumOfDifficulty $ fromIntegral sumDiff
                        , startChunk = (IxChunk 0, Chunk $ MkQuantity sc)
                        , endChunk = (IxChunk 0, Chunk $ MkQuantity ec)
                        }
                , landout = lo
                }
        , \TaskLanding{..} ->
            let (sc', ec', sumDiff') =
                    maybe
                        (0, 0, 0)
                        (\Chunking
                            { sumOf = SumOfDifficulty sumDiff
                            , startChunk = (_, Chunk (MkQuantity sc))
                            , endChunk = (_, Chunk (MkQuantity ec))
                            } -> (sc, ec, sumDiff))
                        chunking
            in
                ( maybe 0 (\(Lookahead ahead) -> ahead) lookahead
                , sc'
                , ec'
                , fromIntegral sumDiff'
                , landout
                )
        )
    $ xp5Tuple
        (xpAttr "look_ahead" xpInt)
        (xpAttr "start_chunks" xpPrim)
        (xpAttr "end_chunks" xpPrim)
        (xpAttr "sum_of_difficulty" xpInt)
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
