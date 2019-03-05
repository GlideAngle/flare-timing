module Flight.Fsdb.TaskScore (parseScores) where

import Data.Maybe (catMaybes)
import Text.XML.HXT.Arrow.Pickle
    ( PU(..)
    , unpickleDoc, xpWrap, xpFilterAttr, xpElem, xpAttr
    , xpPair, xpInt
    )
import Text.XML.HXT.DOM.TypeDefs (XmlTree)
import Text.XML.HXT.Core
    ( ArrowXml
    , (<+>)
    , (&&&)
    , (>>>)
    , runX
    , withValidate
    , withWarnings
    , readString
    , no
    , hasName
    , getChildren
    , getAttrValue
    , constA
    , listA
    , arr
    , deep
    , containing
    , orElse
    , hasAttr
    )

import Flight.Track.Point (NormPointing(..), NormBreakdown(..))
import Flight.Comp (PilotId(..), Pilot(..))
import Flight.Score (TaskPoints(..), TaskPlacing(..))
import Flight.Fsdb.Pilot (getCompPilot)
import Flight.Fsdb.KeyPilot (unKeyPilot, keyPilots, keyMap)

xpRankScore :: PU NormBreakdown
xpRankScore =
    xpElem "FsResult"
    $ xpFilterAttr
        ( hasName "rank"
        <+> hasName "points"
        )
    $ xpWrap
        ( \(rnk, pts) ->
            NormBreakdown
                { place = TaskPlacing . fromIntegral $ rnk
                , total = TaskPoints . toRational $ pts
                }
        , \NormBreakdown
                { place = TaskPlacing rnk
                , total = TaskPoints pts
                } -> (fromIntegral rnk, round pts)
        )
    $ xpPair
        (xpAttr "rank" xpInt)
        (xpAttr "points" xpInt)

getScore :: ArrowXml a => [Pilot] -> a XmlTree [(Pilot, Maybe NormBreakdown)]
getScore pilots =
    getChildren
    >>> deep (hasName "FsTask")
    >>> getPoint
    where
        kps = keyPilots pilots

        -- <FsParticipant id="28">
        --    <FsResult rank="49" points="56" />
        getPoint =
            ( getChildren
            >>> hasName "FsParticipants"
            >>> listA getDidScore
            )
            -- NOTE: If a task is created when there are no participants
            -- then the FsTask/FsParticipants element is omitted.
            `orElse` constA []
            where
                getDidScore =
                    getChildren
                    >>> hasName "FsParticipant"
                        `containing`
                        ( getChildren
                        >>> hasName "FsResult"
                        >>> hasAttr "rank"
                        >>> hasAttr "points"
                        )
                    >>> getAttrValue "id"
                    &&& getResultScore
                    >>> arr (\(pid, x) -> (unKeyPilot (keyMap kps) . PilotId $ pid, x))

                getResultScore =
                    getChildren
                    >>> hasName "FsResult"
                    >>> arr (unpickleDoc xpRankScore)


parseScores :: String -> IO (Either String NormPointing)
parseScores contents = do
    let doc = readString [ withValidate no, withWarnings no ] contents
    ps <- runX $ doc >>> getCompPilot
    xss <- runX $ doc >>> getScore ps
    let ys =
            [
                catMaybes
                $ (\case
                    (a, Just b) -> Just (a, b)
                    (_, Nothing) -> Nothing)
                <$> xs

            | xs <- xss
            ]

    return . Right . NormPointing $ ys
