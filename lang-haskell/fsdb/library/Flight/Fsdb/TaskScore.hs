{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.Fsdb.TaskScore (parseAltScores) where

import Prelude hiding (max)
import qualified Prelude as Stats (max)
import Data.Time.LocalTime (TimeOfDay, timeOfDayToTime)
import Data.Maybe (fromMaybe, catMaybes)
import Data.List (unzip5)
import qualified Data.Map.Strict as Map (fromList, lookup, union)
import qualified Data.Vector as V (fromList)
import qualified Statistics.Sample as Stats (meanVariance)
import Data.UnitsOfMeasure ((*:), u, zero, convert, fromRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Text.XML.HXT.Arrow.Pickle
    ( PU(..)
    , unpickleDoc, unpickleDoc'
    , xpWrap, xpElem, xpAttr
    , xpFilterAttr, xpFilterCont
    , xpInt, xpPrim, xpDefault
    , xpPair, xpTriple, xp4Tuple, xp5Tuple, xp6Tuple, xp14Tuple
    , xpTextAttr, xpOption
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

import Flight.Distance (TaskDistance(..), QTaskDistance, unTaskDistanceAsKm)
import Flight.Track.Distance (AwardedDistance(..))
import Flight.Track.Point (AlternativePointing(..), AltPointing, AltBreakdown(..))
import qualified Flight.Track.Point as Alt (AltBreakdown(..))
import Flight.Comp (PilotId(..), Pilot(..), Nominal(..), DfNoTrackPilot(..))
import qualified "flight-gap-allot" Flight.Score as Frac (Fractions(..))
import "flight-gap-allot" Flight.Score
    ( LaunchToEss(..)
    , NominalGoal(..)
    , NominalLaunch(..)
    , NominalDistance(..)
    , MinimumDistance(..)
    , NominalTime(..), BestTime(..), PilotTime(..)
    , PilotsFlying(..), PilotsPresent(..), PilotsAtEss(..), PilotsLanded(..)
    , FlownMax(..), FlownMean(..), FlownStdDev(..)
    , LinearFraction(..)
    , DifficultyFraction(..)
    , DistanceFraction(..)
    , SpeedFraction(..)
    , ArrivalFraction(..)
    , TaskPlacing(..)
    , SumOfDistance(..)
    )
import "flight-gap-lead" Flight.Score
    ( LeadingArea(..), LeadingCoef(..)
    , LeadingFraction(..)
    )
import "flight-gap-math" Flight.Score
    ( TaskPoints(..)
    , Points(..)
    , LinearPoints(..)
    , DifficultyPoints(..)
    , DistancePoints(..)
    , LeadingPoints(..)
    , ArrivalPoints(..)
    , TimePoints(..)
    )
import "flight-gap-valid" Flight.Score
    ( Validity(..), TaskValidity(..), StopValidity(..)
    , LaunchValidity(..), LaunchValidityWorking(..)
    , DistanceValidity(..), DistanceValidityWorking(..)
    , TimeValidity(..), TimeValidityWorking(..)
    , ReachToggle(..), ReachStats(..), StopValidityWorking(..)
    , NominalDistanceArea(..)
    )
import Flight.Fsdb.Pilot (getCompPilot)
import Flight.Fsdb.KeyPilot (unKeyPilot, keyPilots, keyMap)
import Flight.Fsdb.Internal.Parse (parseUtcTime, parseHmsTime)
import Flight.Fsdb.Distance (asTaskKm, asAwardReach, taskMetresToKm, taskKmToMetres)
import Flight.Fsdb.Task (getDidFly, getDidFlyNoTracklog, asAward)

dToR :: Double -> Rational
dToR = toRational

toPilotTime :: TimeOfDay -> PilotTime (Quantity Double [u| h |])
toPilotTime x =
    PilotTime $ convert secs
    where
        secs :: Quantity Double [u| s |]
        secs = fromRational' . MkQuantity . toRational $ timeOfDayToTime x

xpRankScore :: PU AltBreakdown
xpRankScore =
    xpElem "FsResult"
    $ xpFilterAttr
        ( hasName "rank"
        <+> hasName "points"

        <+> hasName "linear_distance_points"
        <+> hasName "difficulty_distance_points"

        <+> hasName "distance_points"
        <+> hasName "leading_points"
        <+> hasName "arrival_points"
        <+> hasName "time_points"

        <+> hasName "real_distance"
        <+> hasName "distance"
        <+> hasName "last_distance"

        <+> hasName "started_ss"
        <+> hasName "finished_ss"
        <+> hasName "ss_time"
        )
    $ xpWrap
        ( \(r, p, ldp, ddp, dp, l, a, t, dM, dE, dL, ss, es, ssE) ->
            let place = TaskPlacing $ fromIntegral r in

            AltBreakdown
                { placeGiven = place
                -- NOTE: We'll adjust place taken later.
                , placeTaken = place
                , total = TaskPoints p
                , breakdown =
                    Points
                        { reach = LinearPoints $ dToR ldp
                        , effort = DifficultyPoints $ dToR ddp
                        , distance = DistancePoints $ dToR dp
                        , leading = LeadingPoints $ dToR l
                        , arrival = ArrivalPoints $ dToR a
                        , time = TimePoints $ dToR t
                        }
                , fractions =
                    Frac.Fractions
                        { Frac.reach = LinearFraction 0
                        , Frac.effort = DifficultyFraction 0
                        , Frac.distance = DistanceFraction 0
                        , Frac.leading = LeadingFraction 0
                        , Frac.arrival = ArrivalFraction 0
                        , Frac.time = SpeedFraction 0
                        }

                , reach =
                    ReachToggle
                        { flown = Just . taskKmToMetres . TaskDistance $ MkQuantity dM
                        , extra = Just . taskKmToMetres . TaskDistance $ MkQuantity dE
                        }
                -- WARNING: Sometimes FS writes min double to last_distance.
                -- last_distance="-1.79769313486232E+305"
                , landedMade =
                    Just . taskKmToMetres . TaskDistance . MkQuantity
                    $ Stats.max 0 dL
                , ss = parseUtcTime <$> ss
                , es = parseUtcTime <$> es

                -- WARNING: FS always has a time for ss_time but airscore uses
                -- "" instead of "00:00:00" when the pilot didn't complete the
                -- speed section.
                , timeElapsed =
                    if | ssE == Just "00:00:00" -> Nothing
                       | ssE == Just "" -> Nothing
                       | otherwise -> toPilotTime . parseHmsTime <$> ssE

                , leadingArea = Nothing
                , leadingCoef = LeadingCoef zero
                }
        , \AltBreakdown
                { placeGiven = TaskPlacing r
                , total = TaskPoints p
                , breakdown =
                    Points
                        { reach = LinearPoints ldp
                        , effort = DifficultyPoints ddp
                        , distance = DistancePoints dp
                        , leading = LeadingPoints l
                        , arrival = ArrivalPoints a
                        , time = TimePoints t
                        }
                , reach = ReachToggle{flown, extra}
                , landedMade
                , ss
                , es
                , timeElapsed
                } ->
                    ( fromIntegral r
                    , p
                    , fromRational ldp
                    , fromRational ddp
                    , fromRational dp
                    , fromRational l
                    , fromRational a
                    , fromRational t
                    , maybe 0 (\(TaskDistance (MkQuantity dE)) -> dE) extra
                    , maybe 0 (\(TaskDistance (MkQuantity dM)) -> dM) flown
                    , maybe 0 (\(TaskDistance (MkQuantity dL)) -> dL) landedMade
                    , show <$> ss
                    , show <$> es
                    , show <$> timeElapsed
                    )
        )
    $ xp14Tuple
        (xpAttr "rank" xpInt)
        -- NOTE: FS used to write int points but since Apr 2020 writes floats.
        (xpAttr "points" xpPrim)

        (xpDefault (0 :: Double) $ xpAttr "linear_distance_points" xpPrim)
        (xpDefault (0 :: Double) $ xpAttr "difficulty_distance_points" xpPrim)

        (xpAttr "distance_points" xpPrim)
        (xpAttr "leading_points" xpPrim)
        (xpAttr "arrival_points" xpPrim)
        (xpAttr "time_points" xpPrim)

        (xpAttr "real_distance" xpPrim)
        (xpAttr "distance" xpPrim)
        (xpDefault (0 :: Double) $ xpAttr "last_distance" xpPrim)

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
    $ xpFilterCont isAttr
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
    $ xpFilterCont isAttr
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
    $ xpFilterCont isAttr
    $ xpFilterAttr
        (hasName "best_time"
        <+> hasName "best_dist"
        <+> hasName "best_real_dist"
        )
    $ xpWrap
        ( \(bt, bdE, bdF) ->
            TimeValidityWorking
                { ssBestTime = Nothing
                , gsBestTime =
                    if bt == 0 then Nothing else
                    Just . BestTime $ MkQuantity bt
                , nominalTime = nt
                , nominalDistance = nd
                , reachMax =
                    ReachToggle
                        { extra = FlownMax $ MkQuantity bdE
                        , flown = FlownMax $ MkQuantity bdF
                        }
                }
        , \TimeValidityWorking
                { gsBestTime = bt
                , reachMax =
                    ReachToggle
                        { extra = FlownMax (MkQuantity bdE)
                        , flown = FlownMax (MkQuantity bdF)
                        }
                } ->
                    let bt' = maybe 0 (\(BestTime (MkQuantity x)) -> x) bt
                    in (bt', bdE, bdF)
        )
    $ xpTriple
        (xpAttr "best_time" xpPrim)
        (xpAttr "best_dist" xpPrim)
        (xpAttr "best_real_dist" xpPrim)

xpDistanceValidityWorking
    :: NominalGoal
    -> NominalDistance (Quantity Double [u| km |])
    -> MinimumDistance (Quantity Double [u| km |])
    -> PU DistanceValidityWorking
xpDistanceValidityWorking
    ng'@(NominalGoal ngR)
    nd'@(NominalDistance (MkQuantity nd))
    md'@(MinimumDistance (MkQuantity md)) =
    xpElem "FsTaskScoreParams"
    $ xpFilterCont isAttr
    $ xpFilterAttr
        ( hasName "sum_real_dist_over_min"
        <+> hasName "no_of_pilots_flying"
        <+> hasName "best_dist"
        <+> hasName "best_real_dist"
        )
    $ xpWrap
        ( \(sd, pf, bdE, bdF) ->
            DistanceValidityWorking
                { sum = SumOfDistance $ MkQuantity sd
                , flying = PilotsFlying $ fromIntegral pf
                , area =
                    let a :: Double = (ng + 1) * (nd - md)
                        b :: Double = Stats.max 0 $ ng * (bdF - nd)
                    in NominalDistanceArea . toRational $ (a + b) / 2
                , nominalGoal = ng'
                , nominalDistance = nd'
                , minimumDistance = md'
                , reachMax =
                    ReachToggle
                        { extra = FlownMax $ MkQuantity bdE
                        , flown = FlownMax $ MkQuantity bdF
                        }
                }
        , \DistanceValidityWorking
                { sum = SumOfDistance (MkQuantity sd)
                , flying = PilotsFlying pf
                , reachMax =
                    ReachToggle
                        { extra = FlownMax (MkQuantity bdE)
                        , flown = FlownMax (MkQuantity bdF)
                        }
                } ->
                    (sd, fromIntegral pf, bdE, bdF)
        )
    $ xp4Tuple
        (xpAttr "sum_real_dist_over_min" xpPrim)
        (xpAttr "no_of_pilots_flying" xpInt)
        (xpAttr "best_dist" xpPrim)
        (xpAttr "best_real_dist" xpPrim)
    where
        ng = fromRational ngR

xpStopValidityWorking :: PU StopValidityWorking
xpStopValidityWorking =
    xpElem "FsTaskScoreParams"
    $ xpFilterCont isAttr
    $ xpFilterAttr
        ( hasName "no_of_pilots_reaching_es"
        <+> hasName "no_of_pilots_landed_before_stop"
        <+> hasName "no_of_pilots_flying"
        <+> hasName "best_dist"
        <+> hasName "best_real_dist"
        <+> hasName "launch_to_ess_distance"
        )
    $ xpWrap
        ( \(pe, pl, pf, eMax, fMax, ed) ->
            let qExtraMax :: Quantity _ [u| km |] = MkQuantity eMax
                qFlownMax :: Quantity _ [u| km |] = MkQuantity fMax
            in
                StopValidityWorking
                    { pilotsAtEss = PilotsAtEss $ fromIntegral pe
                    , landed = PilotsLanded $ fromIntegral pl
                    , stillFlying = PilotsFlying . fromIntegral $ pf - pl
                    , flying = PilotsFlying $ fromIntegral pf
                    , reachStats =
                        ReachToggle
                            { extra = Just $
                                ReachStats
                                    { max = FlownMax $ convert qExtraMax
                                    , mean = FlownMean [u| 0 km |]
                                    , stdDev = FlownStdDev [u| 0 km |]
                                    }
                            , flown =Just $
                                ReachStats
                                    { max = FlownMax $ convert qFlownMax
                                    , mean = FlownMean [u| 0 km |]
                                    , stdDev = FlownStdDev [u| 0 km |]
                                    }
                            }
                    , launchToEssDistance = LaunchToEss . MkQuantity <$> ed
                    }
        , \StopValidityWorking
                { pilotsAtEss = PilotsAtEss pe
                , landed = PilotsLanded pl
                , flying = PilotsFlying pf
                , reachStats = ReachToggle{extra, flown}
                , launchToEssDistance = ed
                } ->
                    ( fromIntegral pe
                    , fromIntegral pl
                    , fromIntegral pf
                    , maybe
                        0
                        (\ReachStats{max = FlownMax qExtraMax} ->
                            let (MkQuantity eMax) :: Quantity _ [u| km |] = convert qExtraMax
                            in eMax)
                        extra
                    , maybe
                        0
                        (\ReachStats{max = FlownMax qFlownMax} ->
                            let (MkQuantity fMax) :: Quantity _ [u| km |] = convert qFlownMax
                            in fMax)
                        flown
                    , do
                        LaunchToEss (MkQuantity d) <- ed
                        return d
                    )
        )
    $ xp6Tuple
        (xpAttr "no_of_pilots_reaching_es" xpInt)
        (xpDefault 0 $ xpAttr "no_of_pilots_landed_before_stop" xpInt)
        (xpAttr "no_of_pilots_flying" xpInt)
        (xpAttr "best_dist" xpPrim)
        (xpAttr "best_real_dist" xpPrim)
        (xpOption $ xpAttr "launch_to_ess_distance" xpPrim)

getScore :: ArrowXml a => [Pilot] -> a XmlTree [(Pilot, Maybe AltBreakdown)]
getScore pilots =
    getChildren
    >>> deep (hasName "FsTask")
    >>> getAttrValue "name"
    &&& getTaskDistance
    &&& getDidFly kps
    &&& getDidFlyNoTracklog kps
    &&& getPoint
    -- NOTE: wt = with track, nt = no track.
    >>> arr (\(_name, (t, (wt, (nt, xs)))) ->
        let td :: Maybe (QTaskDistance _ [u| km |]) = asTaskKm t
            dfwt = asAward t <$> wt
            dfnt = asAward t <$> nt
            yswt :: [(Pilot, _)] = (\DfNoTrackPilot{awardedReach = ar, ..} -> (pilot, ar)) <$> dfwt
            ysnt :: [(Pilot, _)] = (\DfNoTrackPilot{awardedReach = ar, ..} -> (pilot, ar)) <$> dfnt
            dfwtM = Map.fromList yswt
            dfntM = Map.fromList ysnt
            dfM = Map.union dfntM dfwtM
        in
            [
                (,) p $ do
                    n@AltBreakdown{reach = r@ReachToggle{flown = dm}, fractions = fracs} <- x
                    dKm <- taskMetresToKm <$> dm
                    AwardedDistance{awardedFrac = dFrac} <- asAwardReach t (Just dKm)
                    return
                        n
                            { fractions = fracs{Frac.distance = DistanceFraction $ toRational dFrac}
                            , Alt.reach =
                                maybe
                                    r
                                    (\(TaskDistance (d :: Quantity _ [u| m |])
                                          , ReachToggle
                                              { flown = fracFlown
                                              , extra = fracExtra
                                              }) ->
                                        let fracDistance AwardedDistance{awardedFrac} =
                                                    TaskDistance $ MkQuantity awardedFrac *: d

                                            f = fracDistance fracFlown

    -- WARNING: FS will set FsFlightData/@bonus_distance="0" in cases where the
    -- task is using barometric altitude but the track only has GPS altitude.
    -- Despite this, FS scores reach on the max of FsFlight/@distance and
    -- FsFlightData/@bonus_distance.
                                            e = fracDistance $ Stats.max fracFlown fracExtra
                                        in
                                            r{flown = Just f, extra = Just e})
                                    (do
                                        TaskDistance td' <- td
                                        let td'' :: Quantity _ [u| m |] = convert td'

                                        ad <- Map.lookup p dfM
                                        ad' <- ad

                                        return (TaskDistance td'', ad'))
                            }

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
                        >>> hasAttr "real_distance"
                        >>> hasAttr "distance"
                        )
                    >>> getAttrValue "id"
                    &&& getResultScore
                    &&& getLeading
                    >>> arr (\(pid, (x, ld)) ->
                        ( unKeyPilot (keyMap kps) . PilotId $ pid
                        , do
                            norm <- x
                            return $
                                maybe
                                    norm
                                    (\(a, c) ->
                                        norm
                                            { leadingArea =
                                                Just . LeadingArea . MkQuantity
                                                $ fromIntegral a

                                            , leadingCoef = LeadingCoef . MkQuantity $ c
                                            })
                                    ld
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
            (getChildren
            >>> hasName "FsTaskScoreParams"
            >>> getAttrValue "task_distance"
            )
            `orElse` constA ""

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
             ( Maybe Validity
             , Maybe LaunchValidityWorking
             , Maybe TimeValidityWorking
             , Maybe DistanceValidityWorking
             , Maybe StopValidityWorking
             )
         )
getValidity ng nl nd md nt =
    getChildren
    >>> deep (hasName "FsTask")
    >>> maybeScored getTaskValidity
    &&& maybeScored getLaunchValidityWorking
    &&& maybeScored getTimeValidityWorking
    &&& maybeScored getDistanceValidityWorking
    &&& maybeScored getStopValidityWorking
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

        -- NOTE: When a task is cancelled, it doesn't have FsTaskScoreParams.
        --  <FsTask>
        --    <FsTaskState task_state="CANCELLED">
        --    <!-- No child FsTaskScoreParams element -->
        --  </FsTask>
        maybeScored x = (x >>> arr (fmap Just)) `orElse` constA (Right Nothing)

parseAltScores :: Nominal -> String -> IO (Either String AltPointing)
parseAltScores
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

    let yss :: [[(Pilot, AltBreakdown)]] = [catMaybes $ sequence <$> xs| xs <- xss]
    let tss :: [Maybe (BestTime (Quantity Double [u| h |]))] = const Nothing <$> yss

    let rss :: [([Maybe (QTaskDistance Double [u| m |])], [Maybe (QTaskDistance Double [u| m |])])] =
            [
                unzip
                [ (extra, flown)
                | AltBreakdown{reach = ReachToggle{extra, flown}} <- ys
                ]
            | ys <- (fmap . fmap) snd yss
            ]

    let mkReachStats xs' = do
            xs <- Just $ fromMaybe (TaskDistance zero) <$> xs'
            let ys = V.fromList $ unTaskDistanceAsKm <$> xs
            let (ysMean, ysVar) = Stats.meanVariance ys
            return $
                ReachStats
                    { max = FlownMax $ if null ys then [u| 0 km |] else MkQuantity $ maximum ys
                    , mean = FlownMean $ MkQuantity ysMean
                    , stdDev = FlownStdDev . MkQuantity $ sqrt ysVar
                    }

    let es :: [Maybe ReachStats] = [ mkReachStats xs' | (xs', _) <- rss ]
    let rs :: [Maybe ReachStats] = [ mkReachStats xs' | (_, xs') <- rss ]

    gvs <- runX $ doc >>> getValidity ng nl nd md nt
    let vws :: [Either String (Maybe _, Maybe _, Maybe _, Maybe _, Maybe _)] =
            [
                (\(vs, lw, tw, dw, sw') ->
                    ( vs
                    , lw
                    , tw
                    , dw
                    ,
                        (\sw -> sw{reachStats = ReachToggle{extra = e, flown = r}})
                        <$> sw'
                    )
                )
                <$> gv
            | gv <- gvs
            | e <- es
            | r <- rs
            ]

    return $
        (\(vs, lw, tw, dw, sw) ->
            AlternativePointing
                { bestTime = tss
                , validityWorkingLaunch = lw
                , validityWorkingTime = tw
                , validityWorkingDistance = dw
                , validityWorkingStop = sw
                , validity = vs
                , score = yss
                })
        . unzip5
        <$> sequence vws
