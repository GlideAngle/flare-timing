module Flight.Fsdb.TaskScore (parseScores) where

import Data.UnitsOfMeasure (u, zero, convert, fromRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Data.Time.LocalTime (TimeOfDay, timeOfDayToTime)
import Data.Maybe (catMaybes)

import Text.XML.HXT.Arrow.Pickle
    ( PU(..)
    , unpickleDoc, xpWrap, xpFilterAttr, xpElem, xpAttr
    , xpInt, xpPrim, xpPair, xp10Tuple, xpTextAttr, xpOption
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

import Flight.Distance (TaskDistance(..))
import Flight.Track.Distance (AwardedDistance(..))
import Flight.Track.Point (NormPointing(..), NormBreakdown(..))
import Flight.Comp (PilotId(..), Pilot(..))
import Flight.Score
    ( TaskPoints(..), TaskPlacing(..)
    , DistancePoints(..)
    , LeadingPoints(..)
    , ArrivalPoints(..)
    , TimePoints(..)
    , PilotTime(..)
    , SpeedFraction(..)
    , ArrivalFraction(..)
    , LeadingArea(..), LeadingCoef(..), LeadingFraction(..)
    )
import Flight.Fsdb.Pilot (getCompPilot)
import Flight.Fsdb.KeyPilot (unKeyPilot, keyPilots, keyMap)
import Flight.Fsdb.Internal.Parse (parseUtcTime, parseHmsTime)
import Flight.Fsdb.Distance (asAwardReach, taskMetresToKm, taskKmToMetres)

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
        <+> hasName "distance"
        )
    $ xpWrap
        ( \(r, p, dp, l, a, t, dM, ss, es, ssE) ->
            NormBreakdown
                { place = TaskPlacing . fromIntegral $ r
                , total = TaskPoints . toRational $ p
                , distance = DistancePoints . dToR $ dp
                , leading = LeadingPoints . dToR $ l
                , arrival = ArrivalPoints . dToR $ a
                , time = TimePoints . dToR $ t
                , distanceMade = taskKmToMetres . TaskDistance . MkQuantity $ dM
                , distanceFrac = 0
                , ss = parseUtcTime <$> ss
                , es = parseUtcTime <$> es
                , timeElapsed =
                    if ssE == Just "00:00:00" then Nothing else
                    toPilotTime . parseHmsTime <$> ssE
                , timeFrac = SpeedFraction 0
                , leadingArea = LeadingArea zero
                , leadingCoef = LeadingCoef zero
                , leadingFrac = LeadingFraction 0
                , arrivalFrac = ArrivalFraction 0
                }
        , \NormBreakdown
                { place = TaskPlacing r
                , total = TaskPoints p
                , distance = DistancePoints dp
                , leading = LeadingPoints l
                , arrival = ArrivalPoints a
                , time = TimePoints t
                , distanceMade = TaskDistance (MkQuantity d)
                , ss
                , es
                , timeElapsed
                } ->
                    ( fromIntegral r
                    , round p
                    , fromRational dp
                    , fromRational l
                    , fromRational a
                    , fromRational t
                    , d
                    , show <$> ss
                    , show <$> es
                    , show <$> timeElapsed
                    )
        )
    $ xp10Tuple
        (xpAttr "rank" xpInt)
        (xpAttr "points" xpInt)
        (xpAttr "distance_points" xpPrim)
        (xpAttr "leading_points" xpPrim)
        (xpAttr "arrival_points" xpPrim)
        (xpAttr "time_points" xpPrim)
        (xpAttr "distance" xpPrim)
        (xpOption $ xpTextAttr "started_ss")
        (xpOption $ xpTextAttr "finished_ss")
        (xpOption $ xpTextAttr "ss_time")

xpLeading :: PU (Int, Double)
xpLeading =
    xpElem "FsFlightData"
    $ xpFilterAttr (hasName "iv" <+> hasName "lc")
    $ xpPair
        (xpAttr "iv" xpInt)
        (xpAttr "lc" xpPrim)

getScore :: ArrowXml a => [Pilot] -> a XmlTree [(Pilot, Maybe NormBreakdown)]
getScore pilots =
    getChildren
    >>> deep (hasName "FsTask")
    >>> getTaskDistance
    &&& getPoint
    >>> arr (\(td, xs) ->
        [
            (,) p $ do
                n@NormBreakdown{distanceMade = dm} <- x
                let dKm = taskMetresToKm dm
                AwardedDistance{awardedFrac = frac} <- asAwardReach td (Just dKm)
                return $ n{distanceFrac = frac}

        | (p, x) <- xs
        ])
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
                        >>> hasAttr "distance"
                        )
                    >>> getAttrValue "id"
                    &&& getResultScore
                    &&& getLeading
                    >>> arr (\(pid, (x, ld)) ->
                        ( unKeyPilot (keyMap kps) . PilotId $ pid
                        , do
                            norm <- x
                            (a, c) <- ld
                            return $
                                norm
                                    { leadingArea = LeadingArea . MkQuantity . fromIntegral $ a
                                    , leadingCoef = LeadingCoef . MkQuantity $ c
                                    }
                        ))

                getResultScore =
                    getChildren
                    >>> hasName "FsResult"
                    >>> arr (unpickleDoc xpRankScore)

        getLeading =
            (getChildren
            >>> hasName "FsFlightData"
            >>> arr (unpickleDoc xpLeading)
            )

        getTaskDistance =
            getChildren
            >>> hasName "FsTaskScoreParams"
            >>> getAttrValue "task_distance"

parseScores :: String -> IO (Either String NormPointing)
parseScores contents = do
    let doc = readString [ withValidate no, withWarnings no ] contents
    ps <- runX $ doc >>> getCompPilot
    xss <- runX $ doc >>> getScore ps
    let yss =
            [
                catMaybes
                $ (\case
                    (a, Just b) -> Just (a, b)
                    (_, Nothing) -> Nothing)
                <$> xs

            | xs <- xss
            ]

    let tss = const Nothing <$> yss

    return . Right $ NormPointing{bestTime = tss, score = yss}
