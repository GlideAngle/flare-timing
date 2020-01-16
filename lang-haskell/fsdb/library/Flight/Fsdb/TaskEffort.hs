module Flight.Fsdb.TaskEffort (parseNormLandouts) where

import Data.Either (partitionEithers)
import Data.Map (Map)
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Text.XML.HXT.Arrow.Pickle
    ( PU(..)
    , xpFilterAttr, xpFilterCont
    , xpText, xpInt, xpPrim, unpickleDoc', xpWrap, xpElem, xpAttr
    , xpPair, xp5Tuple, xp9Tuple, xpList
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
    , withRemoveWS
    , readString
    , no
    , yes
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
import Flight.Comp (Pilot(..), PilotId(..))
import Flight.Score
    ( MinimumDistance(..), PilotDistance(..)
    , Lookahead(..), SumOfDifficulty(..)
    , IxChunk(..), Chunk(..), Chunking(..)
    , RelativeDifficulty(..), DifficultyFraction(..)
    , toIxChunk
    )
import qualified Flight.Score as Gap (ChunkDifficulty(..))
import Flight.Fsdb.KeyPilot (unKeyPilot, keyPilots, keyMap)
import Flight.Fsdb.Pilot (getCompPilot)

nullLanding :: MinimumDistance (Quantity Double [u| km |]) -> TaskLanding
nullLanding free =
    TaskLanding
        { minDistance = free
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

xpDown :: PU (String, Double)
xpDown =
    xpElem "FsDown"
    $ xpPair (xpAttr "id" xpText) (xpAttr "down" xpPrim)

xpChunk :: Map PilotId Pilot -> PU Gap.ChunkDifficulty
xpChunk pidMap =
    xpElem "FsChunk"
    $ xpWrap
        ( \(c, s, e, ea, d, dw, rel :: Double, frac :: Double, ds) ->
            nullChunkDifficulty
                { Gap.chunk = IxChunk c
                , Gap.startChunk = Chunk $ MkQuantity s
                , Gap.endChunk = Chunk $ MkQuantity e
                , Gap.endAhead = Chunk $ MkQuantity ea
                , Gap.down = d
                , Gap.downward = dw
                , Gap.downs =
                    PilotDistance
                    . MkQuantity
                    . snd
                    <$> ds
                , Gap.downers = unKeyPilot pidMap . PilotId . fst <$> ds
                , Gap.rel = RelativeDifficulty $ toRational rel
                , Gap.frac = DifficultyFraction $ toRational frac
                }
        , \Gap.ChunkDifficulty
                { Gap.chunk = IxChunk c
                , Gap.startChunk = Chunk (MkQuantity s)
                , Gap.endChunk = Chunk (MkQuantity e)
                , Gap.endAhead = Chunk (MkQuantity ea)
                , Gap.down = d
                , Gap.downward = dw
                , Gap.downs = dns
                , Gap.downers = drs
                , Gap.rel = RelativeDifficulty rel
                , Gap.frac = DifficultyFraction frac
                } ->
                    let ds =
                            [ (pid, pd)
                            | PilotDistance (MkQuantity pd) <- dns
                            | Pilot (PilotId pid, _) <- drs
                            ]
                    in (c, s, e, ea, d, dw, fromRational rel, fromRational frac, ds)
        )
    $ xp9Tuple
        (xpAttr "chunk" xpInt)
        (xpAttr "start" xpPrim)
        (xpAttr "end" xpPrim)
        (xpAttr "end_ahead" xpPrim)
        (xpAttr "down" xpInt)
        (xpAttr "downward" xpInt)
        (xpAttr "rel" xpPrim)
        (xpAttr "frac" xpPrim)
        (xpList xpDown)

ixc :: Double -> (IxChunk, Chunk (Quantity Double [u| km |]))
ixc x = let q = MkQuantity x in (toIxChunk $ PilotDistance q, Chunk q)

xpDifficulty :: MinimumDistance (Quantity Double [u| km |]) -> PU TaskLanding
xpDifficulty free =
    xpElem "FsTaskDifficulty"
    -- WARNING: Filter only attributes, ignoring child elements such as
    -- <FsChunk ... />. If not then the pickling will fail with
    -- "xpCheckEmptyContents: unprocessed XML content detected".
    $ xpFilterCont isAttr
    $ xpFilterAttr
        ( hasName "look_ahead"
        <+> hasName "start_chunks"
        <+> hasName "end_chunks"
        <+> hasName "sum_of_difficulty"
        <+> hasName "no_of_pilots_lo"
        )
    $ xpWrap
        ( \(ahead, sc, ec, sumDiff, lo) ->
            (nullLanding free)
                { lookahead = Just $ Lookahead ahead
                , chunking =
                    Just $
                    Chunking
                        { sumOf = SumOfDifficulty $ fromIntegral sumDiff
                        , startChunk = ixc sc
                        , endChunk = ixc ec
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

getEffort
    :: ArrowXml a
    => MinimumDistance (Quantity Double [u| km |])
    -> [Pilot]
    -> a XmlTree (Either String TaskLanding)
getEffort free pilots =
    getChildren
    >>> deep (hasName "FsTaskDifficulty")
    >>> arr (unpickleDoc' $ xpDifficulty free)
    &&& listA getChunk
    >>> arr (\case
        (Left s, _) -> Left s
        (Right x, cs) ->
            case partitionEithers cs of
                (s : _, _) -> Left s
                ([], rs) -> Right $ x{difficulty = Just rs})
    where
        kps = keyMap $ keyPilots pilots

        getChunk =
            getChildren
            >>> hasName "FsChunk"
            >>> arr (unpickleDoc' $ xpChunk kps)

parseNormLandouts
    :: MinimumDistance (Quantity Double [u| km |])
    -> String
    -> IO (Either String [TaskLanding])
parseNormLandouts free contents = do
    let doc =
            readString
                [ withValidate no
                , withWarnings no
                , withRemoveWS yes
                ]
                contents

    ps <- runX $ doc >>> getCompPilot
    xs <- runX $ doc >>> getEffort free ps
    return $ sequence xs
