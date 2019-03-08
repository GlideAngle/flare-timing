module Flight.Fsdb.TaskScore (parseScores) where

import Data.UnitsOfMeasure (u, convert, fromRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Data.Time.LocalTime (TimeOfDay, timeOfDayToTime)
import Data.Maybe (catMaybes)

import Text.XML.HXT.Arrow.Pickle
    ( PU(..)
    , unpickleDoc, xpWrap, xpFilterAttr, xpElem, xpAttr
    , xpInt, xpPrim, xp9Tuple, xpTextAttr, xpOption
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
import Flight.Score
    ( TaskPoints(..), TaskPlacing(..)
    , DistancePoints(..)
    , LeadingPoints(..)
    , ArrivalPoints(..)
    , TimePoints(..)
    , PilotTime(..)
    )
import Flight.Fsdb.Pilot (getCompPilot)
import Flight.Fsdb.KeyPilot (unKeyPilot, keyPilots, keyMap)
import Flight.Fsdb.Internal.Parse (parseUtcTime, parseHmsTime)

dToR :: Double -> Rational
dToR = toRational

toPilotTime :: TimeOfDay -> PilotTime (Quantity Double [u| h |])
toPilotTime x =
    PilotTime $ convert secs
    where
        secs :: Quantity Double [u| s |]
        secs = fromRational' . MkQuantity . toRational $ timeOfDayToTime x

xpRankScore :: PU NormBreakdown
xpRankScore =
    xpElem "FsResult"
    $ xpFilterAttr
        ( hasName "rank"
        <+> hasName "points"
        <+> hasName "distance_points"
        <+> hasName "leading_points"
        <+> hasName "arrival_points"
        <+> hasName "time_points"
        <+> hasName "started_ss"
        <+> hasName "finished_ss"
        <+> hasName "ss_time"
        )
    $ xpWrap
        ( \(r, p, d, l, a, t, ss, es, ssE) ->
            NormBreakdown
                { place = TaskPlacing . fromIntegral $ r
                , total = TaskPoints . toRational $ p
                , distance = DistancePoints . dToR $ d
                , leading = LeadingPoints . dToR $ l
                , arrival = ArrivalPoints . dToR $ a
                , time = TimePoints . dToR $ t
                , ss = parseUtcTime <$> ss
                , es = parseUtcTime <$> es
                , ssElapsed =
                    if ssE == Just "00:00:00" then Nothing else
                    toPilotTime . parseHmsTime <$> ssE
                }
        , \NormBreakdown
                { place = TaskPlacing r
                , total = TaskPoints p
                , distance = DistancePoints d
                , leading = LeadingPoints l
                , arrival = ArrivalPoints a
                , time = TimePoints t
                , ss
                , es
                , ssElapsed
                } ->
                    ( fromIntegral r
                    , round p
                    , fromRational d
                    , fromRational l
                    , fromRational a
                    , fromRational t
                    , show <$> ss
                    , show <$> es
                    , show <$> ssElapsed
                    )
        )
    $ xp9Tuple
        (xpAttr "rank" xpInt)
        (xpAttr "points" xpInt)
        (xpAttr "distance_points" xpPrim)
        (xpAttr "leading_points" xpPrim)
        (xpAttr "arrival_points" xpPrim)
        (xpAttr "time_points" xpPrim)
        (xpOption $ xpTextAttr "started_ss")
        (xpOption $ xpTextAttr "finished_ss")
        (xpOption $ xpTextAttr "ss_time")

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
                        >>> hasAttr "distance_points"
                        >>> hasAttr "leading_points"
                        >>> hasAttr "arrival_points"
                        >>> hasAttr "time_points"
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
