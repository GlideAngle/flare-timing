module Flight.Fsdb.TaskScore (parseScores) where

import Data.UnitsOfMeasure (u, zero, convert, fromRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Data.Time.LocalTime (TimeOfDay, timeOfDayToTime)
import Data.Maybe (catMaybes)

import Text.XML.HXT.Arrow.Pickle
    ( PU(..)
    , unpickleDoc, unpickleDoc'
    , xpWrap, xpElem, xpAttr
    , xpFilterAttr, xpFilterCont
    , xpInt, xpPrim, xpPair, xp5Tuple, xp10Tuple, xpTextAttr, xpOption
    )
import Text.XML.HXT.DOM.TypeDefs (XmlTree)
import Text.XML.HXT.Core
    ( ArrowXml
    , (<+>)
    , (&&&)
    , (>>>)
    , runX
    , no, yes
    , withValidate, withWarnings, withRemoveWS
    , readString
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
    , isAttr
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
    , Validity(..), TaskValidity(..), StopValidity(..)
    , LaunchValidity(..), DistanceValidity(..), TimeValidity(..)
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

xpValidity :: PU Validity
xpValidity =
    xpElem "FsTaskScoreParams"
    -- WARNING: Filter only attributes, ignoring child elements such as
    -- <FsTaskDistToTp tp_no="1" distance="0.000" />. If not then the pickling
    -- will fail with "xpCheckEmptyContents: unprocessed XML content detected".
    $ xpFilterCont(isAttr)
    $ xpFilterAttr
        ( hasName "day_quality"
        <+> hasName "launch_validity"
        <+> hasName "distance_validity"
        <+> hasName "time_validity"
        <+> hasName "stop_validity"
        )
    $ xpWrap
        ( \(dq, lv, dv, tv, sv) ->
            Validity
                { task = TaskValidity $ dToR dq
                , launch = LaunchValidity $ dToR lv
                , distance = DistanceValidity $ dToR dv
                , time = TimeValidity $ dToR tv
                , stop = StopValidity . dToR <$> sv
                }
        , \Validity
                { task = TaskValidity dq
                , launch = LaunchValidity lv
                , distance = DistanceValidity dv
                , time = TimeValidity tv
                , stop = sv
                } ->
                    ( fromRational dq
                    , fromRational lv
                    , fromRational dv
                    , fromRational tv
                    , (\(StopValidity v) -> fromRational v) <$> sv
                    )
        )
    $ xp5Tuple
        (xpAttr "day_quality" xpPrim)
        (xpAttr "launch_validity" xpPrim)
        (xpAttr "distance_validity" xpPrim)
        (xpAttr "time_validity" xpPrim)
        (xpOption $ xpAttr "stop_validity" xpPrim)


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
            getChildren
            >>> hasName "FsFlightData"
            >>> arr (unpickleDoc xpLeading)

        getTaskDistance =
            getChildren
            >>> hasName "FsTaskScoreParams"
            >>> getAttrValue "task_distance"

getValidity :: ArrowXml a => a XmlTree (Either String Validity)
getValidity =
    getChildren
    >>> deep (hasName "FsTask")
    >>> getTaskValidity
    where
        getTaskValidity =
            getChildren
            >>> hasName "FsTaskScoreParams"
            >>> arr (unpickleDoc' xpValidity)

parseScores :: String -> IO (Either String NormPointing)
parseScores contents = do
    let doc =
            readString
                [ withValidate no
                , withWarnings no
                , withRemoveWS yes
                ]
                contents

    ps <- runX $ doc >>> getCompPilot
    xss <- runX $ doc >>> getScore ps

    let yss = [catMaybes $ sequence <$> xs| xs <- xss]
    let tss = const Nothing <$> yss

    vs :: [Either String Validity] <- runX $ doc >>> getValidity
    return $
        (\vs' -> NormPointing
            { bestTime = tss
            , validity = Just <$> vs'
            , score = yss
            })
        <$> sequence vs
