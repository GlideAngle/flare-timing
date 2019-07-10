module Flight.Fsdb.TaskEffort (parseNormEfforts) where

import Data.Either (partitionEithers)
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Text.XML.HXT.Arrow.Pickle
    ( PU(..)
    , xpFilterAttr, xpFilterCont
    , xpInt, xpPrim, unpickleDoc', xpWrap, xpElem, xpAttr
    , xp5Tuple, xp6Tuple
    )
import Text.XML.HXT.DOM.TypeDefs (XmlTree)
import Text.XML.HXT.Core
    ( ArrowXml
    , (<+>)
    , (>>>)
    , (&&&)
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
    , listA
    )

import Flight.Units ()
import Flight.Fsdb.Internal.XmlPickle ()
import Flight.Track.Land (TaskLanding(..))
import Flight.Score
    ( MinimumDistance(..), Lookahead(..), SumOfDifficulty(..)
    , IxChunk(..), Chunk(..), Chunking(..)
    , RelativeDifficulty(..), DifficultyFraction(..)
    )
import qualified Flight.Score as Gap (ChunkDifficulty(..))

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

nullChunkDifficulty :: Gap.ChunkDifficulty
nullChunkDifficulty =
    Gap.ChunkDifficulty
        { Gap.chunk = IxChunk 0
        , Gap.startChunk = Chunk [u| 0 km |]
        , Gap.endChunk = Chunk [u| 0 km |]
        , Gap.endAhead = Chunk [u| 0 km |]
        , Gap.down = 0
        , Gap.downs = []
        , Gap.downers = []
        , Gap.downward = 0
        , Gap.rel = RelativeDifficulty 0
        , Gap.frac = DifficultyFraction 0
        }

xpChunk :: PU Gap.ChunkDifficulty
xpChunk =
    xpElem "FsChunk"
    -- WARNING: Filter only attributes, ignoring child elements such as
    -- <FsChunk ... />. If not then the pickling will fail with
    -- "xpCheckEmptyContents: unprocessed XML content detected".
    $ xpFilterCont(isAttr)
    $ xpFilterAttr
        ( hasName "chunk"
        <+> hasName "start"
        <+> hasName "end"
        <+> hasName "end_ahead"
        <+> hasName "down"
        <+> hasName "downward"
        )
    $ xpWrap
        ( \(c, s, e, ea, d, dw) ->
            nullChunkDifficulty
                { Gap.chunk = IxChunk c
                , Gap.startChunk = Chunk $ MkQuantity s
                , Gap.endChunk = Chunk $ MkQuantity e
                , Gap.endAhead = Chunk $ MkQuantity ea
                , Gap.down = d
                , Gap.downward = dw
                }
        , \Gap.ChunkDifficulty
                { Gap.chunk = IxChunk c
                , Gap.startChunk = Chunk (MkQuantity s)
                , Gap.endChunk = Chunk (MkQuantity e)
                , Gap.endAhead = Chunk (MkQuantity ea)
                , Gap.down = d
                , Gap.downward = dw
                } -> ( c, s, e, ea, d, dw)
        )
    $ xp6Tuple
        (xpAttr "chunk" xpInt)
        (xpAttr "start" xpPrim)
        (xpAttr "end" xpPrim)
        (xpAttr "end_ahead" xpPrim)
        (xpAttr "down" xpInt)
        (xpAttr "downward" xpInt)

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
    &&& listA getChunk
    >>> arr (\case
        (Left s, _) -> Left s
        (Right x, cs) ->
            case partitionEithers cs of
                (s : _, _) -> Left s
                ([], rs) -> Right $ x{difficulty = Just rs})
    where
        getChunk =
            getChildren
            >>> hasName "FsChunk"
            >>> arr (unpickleDoc' xpChunk)

parseNormEfforts :: String -> IO (Either String [TaskLanding])
parseNormEfforts contents = do
    let doc = readString [ withValidate no, withWarnings no ] contents
    xs <- runX $ doc >>> getEffort
    return $ sequence xs
