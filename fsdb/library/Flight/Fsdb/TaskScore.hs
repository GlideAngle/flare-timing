module Flight.Fsdb.TaskScore (parseScores) where

import Data.Time.LocalTime (TimeOfDay, timeOfDayToTime)
import Data.Maybe (catMaybes)
import Data.List (unzip5)
import Data.UnitsOfMeasure (u, zero, convert, fromRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Text.XML.HXT.Arrow.Pickle
    ( PU(..)
    , unpickleDoc, unpickleDoc'
    , xpWrap, xpElem, xpAttr
    , xpFilterAttr, xpFilterCont
    , xpInt, xpPrim, xpPair, xpTriple, xp5Tuple, xp10Tuple, xpTextAttr, xpOption
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
import Flight.Comp (PilotId(..), Pilot(..), Nominal(..))
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
    , LaunchValidity(..), LaunchValidityWorking(..)
    , DistanceValidity(..), DistanceValidityWorking(..)
    , TimeValidity(..), TimeValidityWorking(..)
    , StopValidityWorking(..)
    , PilotsFlying(..), PilotsPresent(..), PilotsAtEss(..), PilotsLanded(..)
    , FlownMax(..), FlownMean(..), FlownStdDev(..)
    , LaunchToEss(..)
    , NominalGoal(..)
    , NominalLaunch(..)
    , NominalDistance(..), BestDistance(..)
    , MinimumDistance(..), MaximumDistance(..)
    , SumOfDistance(..), NominalDistanceArea(..)
    , NominalTime(..), BestTime(..)
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

xpLaunchValidityWorking :: NominalLaunch -> PU LaunchValidityWorking
xpLaunchValidityWorking nl =
    xpElem "FsTaskScoreParams"
    $ xpFilterCont(isAttr)
    $ xpFilterAttr
        (hasName "no_of_pilots_flying" <+> hasName "no_of_pilots_present")
    $ xpWrap
        ( \(pf, pp) ->
            LaunchValidityWorking
                { flying = PilotsFlying $ fromIntegral pf
                , present = PilotsPresent $ fromIntegral pp
                , nominalLaunch = nl
                }
        , \LaunchValidityWorking
                { flying = PilotsFlying pf
                , present = PilotsPresent pp
                } ->
                    (fromIntegral pf, fromIntegral pp)
        )
    $ xpPair
        (xpAttr "no_of_pilots_flying" xpInt)
        (xpAttr "no_of_pilots_present" xpInt)

xpTimeValidityWorking
    :: NominalDistance (Quantity Double [u| km |])
    -> NominalTime (Quantity Double [u| h |])
    -> PU TimeValidityWorking
xpTimeValidityWorking nd nt =
    xpElem "FsTaskScoreParams"
    $ xpFilterCont(isAttr)
    $ xpFilterAttr
        (hasName "best_dist" <+> hasName "best_time")
    $ xpWrap
        ( \(bd, bt) ->
            TimeValidityWorking
                { ssBestTime = Nothing
                , gsBestTime =
                    if bt == 0 then Nothing else
                    Just . BestTime $ MkQuantity bt
                , bestDistance = BestDistance $ MkQuantity bd
                , nominalTime = nt
                , nominalDistance = nd
                }
        , \TimeValidityWorking
                { gsBestTime = bt
                , bestDistance = BestDistance (MkQuantity bd)
                } ->
                    let bt' = maybe 0 (\(BestTime (MkQuantity x)) -> x) bt
                    in (bd, bt')
        )
    $ xpPair
        (xpAttr "best_dist" xpPrim)
        (xpAttr "best_time" xpPrim)

xpDistanceValidityWorking
    :: NominalGoal
    -> NominalDistance (Quantity Double [u| km |])
    -> MinimumDistance (Quantity Double [u| km |])
    -> PU DistanceValidityWorking
xpDistanceValidityWorking ng nd md =
    xpElem "FsTaskScoreParams"
    $ xpFilterCont(isAttr)
    $ xpFilterAttr
        ( hasName "sum_real_dist_over_min"
        <+> hasName "no_of_pilots_flying"
        <+> hasName "best_dist"
        )
    $ xpWrap
        ( \(sd, pf, bd) ->
            DistanceValidityWorking
                { sum = SumOfDistance $ MkQuantity sd
                , flying = PilotsFlying $ fromIntegral pf
                , area = NominalDistanceArea 0
                , nominalGoal = ng
                , nominalDistance = nd
                , minimumDistance = md
                , bestDistance = MaximumDistance $ MkQuantity bd
                }
        , \DistanceValidityWorking
                { sum = SumOfDistance (MkQuantity sd)
                , flying = PilotsFlying pf
                , bestDistance = MaximumDistance (MkQuantity bd)
                } ->
                    (sd, fromIntegral pf, bd)
        )
    $ xpTriple
        (xpAttr "sum_real_dist_over_min" xpPrim)
        (xpAttr "no_of_pilots_flying" xpInt)
        (xpAttr "best_dist" xpPrim)

xpStopValidityWorking :: PU StopValidityWorking
xpStopValidityWorking =
    xpElem "FsTaskScoreParams"
    $ xpFilterCont(isAttr)
    $ xpFilterAttr
        ( hasName "no_of_pilots_reaching_es"
        <+> hasName "no_of_pilots_landed_before_stop"
        <+> hasName "no_of_pilots_flying"
        <+> hasName "best_dist"
        <+> hasName "launch_to_ess_distance"
        )
    $ xpWrap
        ( \(pe, pl, pf, bd, ed) ->
            StopValidityWorking
                { pilotsAtEss = PilotsAtEss $ fromIntegral pe
                , landed = PilotsLanded $ fromIntegral pl
                , stillFlying = PilotsFlying . fromIntegral $ pf - pl
                , flying = PilotsFlying $ fromIntegral pf
                , flownMax = FlownMax $ MkQuantity bd
                , flownMean = FlownMean [u| 0 km |]
                , flownStdDev = FlownStdDev [u| 0 km |]
                , launchToEssDistance = LaunchToEss $ MkQuantity ed
                }
        , \StopValidityWorking
                { pilotsAtEss = PilotsAtEss pe
                , landed = PilotsLanded pl
                , flying = PilotsFlying pf
                , flownMax = FlownMax (MkQuantity bd)
                , launchToEssDistance = LaunchToEss (MkQuantity ed)
                } ->
                    (fromIntegral pe, fromIntegral pl, fromIntegral pf, bd, ed)
        )
    $ xp5Tuple
        (xpAttr "no_of_pilots_reaching_es" xpInt)
        (xpAttr "no_of_pilots_landed_before_stop" xpInt)
        (xpAttr "no_of_pilots_flying" xpInt)
        (xpAttr "best_dist" xpPrim)
        (xpAttr "launch_to_ess_distance" xpPrim)

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

getValidity
    :: ArrowXml a
    => NominalGoal
    -> NominalLaunch
    -> NominalDistance (Quantity Double [u| km |])
    -> MinimumDistance (Quantity Double [u| km |])
    -> NominalTime (Quantity Double [u| h |])
    -> a XmlTree
         ( Either
             String
             ( Validity
             , LaunchValidityWorking
             , TimeValidityWorking
             , DistanceValidityWorking
             , StopValidityWorking
             )
         )
getValidity ng nl nd md nt =
    getChildren
    >>> deep (hasName "FsTask")
    >>> getTaskValidity
    &&& getLaunchValidityWorking
    &&& getTimeValidityWorking
    &&& getDistanceValidityWorking
    &&& getStopValidityWorking
    >>> arr (\(tv, (lw, (tw, (dw, sw)))) -> do
            tv' <- tv
            lw' <- lw
            tw' <- tw
            dw' <- dw
            sw' <- sw
            return (tv', lw', tw', dw', sw'))
    where
        getTaskValidity =
            getChildren
            >>> hasName "FsTaskScoreParams"
            >>> arr (unpickleDoc' xpValidity)

        getLaunchValidityWorking =
            getChildren
            >>> hasName "FsTaskScoreParams"
            >>> arr (unpickleDoc' $ xpLaunchValidityWorking nl)

        getTimeValidityWorking =
            getChildren
            >>> hasName "FsTaskScoreParams"
            >>> arr (unpickleDoc' $ xpTimeValidityWorking nd nt)

        getDistanceValidityWorking =
            getChildren
            >>> hasName "FsTaskScoreParams"
            >>> arr (unpickleDoc' $ xpDistanceValidityWorking ng nd md)

        getStopValidityWorking =
            getChildren
            >>> hasName "FsTaskScoreParams"
            >>> arr (unpickleDoc' xpStopValidityWorking)

parseScores :: Nominal -> String -> IO (Either String NormPointing)
parseScores
    Nominal
        { goal = ng
        , launch = nl
        , distance = nd
        , time = nt
        , free = md
        }
    contents = do
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

    gvs <- runX $ doc >>> getValidity ng nl nd md nt
    return $
        (\(vs, lw, tw, dw, sw) -> NormPointing
            { bestTime = tss
            , validityWorkingLaunch = Just <$> lw
            , validityWorkingTime = Just <$> tw
            , validityWorkingDistance = Just <$> dw
            , validityWorkingStop = Just <$> sw
            , validity = Just <$> vs
            , score = yss
            })
        . unzip5
        <$> sequence gvs
