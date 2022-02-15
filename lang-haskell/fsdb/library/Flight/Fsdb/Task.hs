{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.Fsdb.Task
    ( parseTasks
    , parseTaskPilotGroups
    , parseTaskPilotPenaltiesAuto
    , parseTaskPilotPenalties
    , getDidFly
    , getDidFlyNoTracklog
    , asAward
    ) where

import Data.Refined (assumeProp, refined)
import Data.Time.Clock (UTCTime, addUTCTime, diffUTCTime)
import Data.Maybe (catMaybes, fromMaybe)
import Data.List (sort, sortOn, nub)
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Text.XML.HXT.Arrow.Pickle
    ( XmlPickler(..), PU(..)
    , xpickle, unpickleDoc, xpWrap, xpFilterAttr, xpFilterCont
    , xpElem, xpAttr, xpAlt, xpDefault
    , xpPair, xpTriple, xp5Tuple, xpInt, xpTrees, xpAttrFixed, xpSeq'
    , xpPrim, xpAttrImplied, xpTextAttr
    )
import Text.XML.HXT.DOM.TypeDefs (XmlTree)
import Text.XML.HXT.Core
    ( ArrowXml
    , (<+>)
    , (&&&)
    , (>>>)
    , (/>)
    , runX
    , withValidate
    , withWarnings
    , readString
    , no
    , hasName
    , getChildren
    , getAttrValue
    , hasAttrValue
    , constA
    , listA
    , arr
    , deep
    , notContaining
    , containing
    , orElse
    , hasAttr
    , isAttr
    , neg
    )

import Flight.Units ()
import Flight.Distance (TaskDistance(..), QTaskDistance)
import Flight.LatLng (LatLng(..), Lat(..), Lng(..), Alt(..), QAlt)
import Flight.LatLng.Raw (RawLat(..), RawLng(..))
import Flight.Zone (Radius(..), AltTime(..))
import Flight.Zone.MkZones
    ( Discipline(..), Decelerator(..), CessIncline(..) , GoalLine(..)
    , mkZones
    )
import qualified Flight.Zone.Raw as Z (RawZone(..))
import Flight.Track.Time (AwardedVelocity(..))
import Flight.Comp
    ( PilotId(..), Pilot(..)
    , PilotGroup(..), DfNoTrack(..), DfNoTrackPilot(..)
    , Task(..), TaskStop(..), StartGate(..), OpenClose(..), Tweak(..)
    , EarlyStart(..), nullEarlyStart
    )
import "flight-gap-math" Flight.Score
    ( SecondsPerPoint(..), JumpTheGunLimit(..), TooEarlyPoints(..)
    , PenaltySeq(..)
    , idMul, idSeq, addSeq, mulSeq
    , mkMul, mkAdd, exMul, exAdd
    )
import "flight-gap-stop" Flight.Score (ScoreBackTime(..))
import "flight-gap-valid" Flight.Score (ReachToggle(..))
import Flight.Fsdb.Pilot (getCompPilot)
import Flight.Fsdb.Internal.Parse (parseUtcTime)
import Flight.Fsdb.Internal.XmlPickle (xpNewtypeQuantity)
import Flight.Fsdb.Tweak (xpTweak)
import Flight.Fsdb.KeyPilot (KeyPilot, unKeyPilot, keyPilots, keyMap)
import Flight.Fsdb.Distance (asAwardReach)

-- | The attribute //FsTaskDefinition@goal.
newtype FsGoal = FsGoal String

instance (u ~ Quantity Double [u| m |]) => XmlPickler (Radius u) where
    xpickle = xpNewtypeQuantity

instance (u ~ Quantity Double [u| m |]) => XmlPickler (Alt u) where
    xpickle = xpNewtypeQuantity

instance (u ~ Quantity Double [u| km |]) => XmlPickler (TaskDistance u) where
    xpickle = xpNewtypeQuantity

xpPointPenaltyAuto :: PU (PenaltySeq, String)
xpPointPenaltyAuto =
    xpElem "FsResult"
    $ xpFilterAttr
        ( hasName "penalty_points_auto"
        <+> hasName "penalty_reason_auto"
        )
    $ xpWrap
        ( \case
            (0.0, s) -> (idSeq, s)
            (pts, s) -> (addSeq $ negate pts, s)
        , \(PenaltySeq{add}, s) -> (exAdd $ negate add, s)
        )
    $ xpPair
        (xpAttr "penalty_points_auto" xpPrim)
        (xpTextAttr "penalty_reason_auto")

xpPointPenalty :: PU (PenaltySeq, String)
xpPointPenalty =
    xpElem "FsResultPenalty"
    $ xpFilterAttr
        ( hasName "penalty"
        <+> hasName "penalty_points"
        <+> hasName "penalty_reason"
        )
    $ xpWrap
        ( \case
            -- WARNING: Zero for the penalty fraction means don't apply
            -- a fraction. If this were a fraction we'd be multiplying by one.
            -- NOTE: We convert penalties to scalings when fractions with f(x) = 1 - x.
            (0.0, 0.0, s) -> (idSeq, s)
            (frac, 0.0, s) -> (mulSeq (1.0 - frac), s)
            (0.0, pts, s) -> (addSeq $ negate pts, s)
            (frac, pts, s) -> (idSeq{mul = mkMul (1.0 - frac), add = mkAdd $ negate pts}, s)
        , \(PenaltySeq{mul, add = y}, s) ->
                case mul of
                    (idMul -> True) -> (0.0, exAdd $ negate y, s)
                    x -> (1.0 - exMul x, exAdd $ negate y, s)
        )
    $ xpTriple
        (xpAttr "penalty" xpPrim)
        (xpAttr "penalty_points" xpPrim)
        (xpTextAttr "penalty_reason")

xpEarlyStart :: PU EarlyStart
xpEarlyStart =
    xpElem "FsScoreFormula"
    $ xpFilterCont isAttr
    $ xpFilterAttr
        ( hasName "jump_the_gun_max"
        <+> hasName "jump_the_gun_factor"
        )
    $ xpWrap
        ( \(limit, rate) ->
            EarlyStart
                (JumpTheGunLimit . MkQuantity $ fromIntegral limit)
                (SecondsPerPoint . MkQuantity $ fromIntegral rate)
                (TooEarlyPoints (assumeProp $ refined 0))
        , \EarlyStart
            { earliest = JumpTheGunLimit (MkQuantity limit)
            , earlyPenalty = SecondsPerPoint (MkQuantity rate)
            } -> (round limit, round rate)
        )
    $ xpPair
        (xpDefault 300 $ xpAttr "jump_the_gun_max" xpInt)
        (xpDefault 2 $ xpAttr "jump_the_gun_factor" xpInt)

xpDecelerator :: PU Decelerator
xpDecelerator =
    xpElem "FsScoreFormula" $ xpAlt tag ps
    where
        tag (CESS _) = 0
        tag (AATB _) = 1
        tag (NoDecelerator _) = 2
        ps = [c, a, n]

        c =
            xpFilterAttr
                ( hasName "final_glide_decelerator"
                <+> hasName "ess_incline_ratio"
                )
            $ xpWrap
                ( CESS . CessIncline . abs
                , \(CESS (CessIncline x)) -> x
                )
            $ xpSeq'
                (xpAttrFixed "final_glide_decelerator" "cess")
                (xpAttr "ess_incline_ratio" xpPrim)

        a =
            xpFilterAttr
                ( hasName "final_glide_decelerator"
                <+> hasName "aatb_factor"
                )
            $ xpWrap
                ( AATB . AltTime . MkQuantity
                , \(AATB (AltTime (MkQuantity r))) -> r
                )
            $ xpSeq'
                (xpAttrFixed "final_glide_decelerator" "aatb")
                (xpAttr "aatb_factor" xpPrim)

        n =
            xpFilterAttr
                ( hasName "final_glide_decelerator"
                <+> hasName "no_final_glide_decelerator_reason"
                )
            $ xpWrap (NoDecelerator, \(NoDecelerator s) -> s)
            $ xpSeq'
                (xpAttrFixed "final_glide_decelerator" "none")
                (xpTextAttr "no_final_glide_decelerator_reason")

xpZone :: PU Z.RawZone
xpZone =
    xpElem "FsTurnpoint"
    $ xpFilterAttr
        ( hasName "id"
        <+> hasName "lat"
        <+> hasName "lon"
        <+> hasName "radius"
        <+> hasName "altitude"
        )
    $ xpWrap
        ( \(n, lat, lng, r, alt) -> Z.RawZone n lat lng r Nothing Nothing alt
        , \Z.RawZone{..} -> (zoneName, lat, lng, radius, alt)
        )
    $ xp5Tuple
        (xpTextAttr "id")
        (xpAttr "lat" xpickle)
        (xpAttr "lon" xpickle)
        (xpAttr "radius" xpickle)
        (xpAttrImplied "altitude" xpickle)

xpHeading :: PU (LatLng Rational [u| deg |])
xpHeading =
    xpElem "FsHeadingpoint"
    $ xpFilterAttr (hasName "lat" <+> hasName "lon")
    $ xpWrap
        ( \(RawLat lat, RawLng lng) ->
            LatLng (Lat . MkQuantity $ lat, Lng . MkQuantity $ lng)
        , \(LatLng (Lat (MkQuantity lat), Lng (MkQuantity lng))) ->
            (RawLat lat, RawLng lng)
        )
    $ xpPair
        (xpAttr "lat" xpickle)
        (xpAttr "lon" xpickle)

xpZoneAltitude :: PU (QAlt Double [u| m |])
xpZoneAltitude =
    xpElem "FsTurnpoint"
    $ xpFilterAttr (hasName "altitude")
    $ xpAttr "altitude" xpickle

xpOpenClose :: PU OpenClose
xpOpenClose =
    xpElem "FsTurnpoint"
    $ xpFilterAttr (hasName "open" <+> hasName "close")
    $ xpWrap
        ( \(open, close) -> OpenClose (parseUtcTime open) (parseUtcTime close)
        , \OpenClose{..} -> (show open, show close)
        )
    $ xpPair
        (xpTextAttr "open")
        (xpTextAttr "close")

xpStartGate :: PU StartGate
xpStartGate =
    xpElem "FsStartGate"
    $ xpFilterAttr (hasName "open")
    $ xpWrap
        ( StartGate . parseUtcTime
        , \(StartGate open) -> show open
        )
    $ xpTextAttr "open"

xpSpeedSection :: PU (Int, Int)
xpSpeedSection =
    xpElem "FsTaskDefinition"
    $ xpFilterAttr (hasName "ss" <+> hasName "es")
    $ xpWrap
        ( \(a, b, _) -> (a, b)
        , \(a, b) -> (a, b, [])
        )
    $ xpTriple
        (xpAttr "ss" xpInt)
        (xpAttr "es" xpInt)
        xpTrees

data TaskState
    = TaskStateRegular UTCTime
    | TaskStateCancel UTCTime
    | TaskStateStop UTCTime

xpStopped :: PU TaskState
xpStopped =
    xpElem "FsTaskState" $ xpAlt tag ps
    where
        tag (TaskStateRegular _) = 0
        tag (TaskStateCancel _) = 1
        tag (TaskStateStop _) = 2
        ps = [r, c, s]

        r =
            xpFilterAttr (hasName "task_state" <+> hasName "stop_time")
            $ xpWrap
                ( TaskStateRegular . parseUtcTime
                , \(TaskStateRegular t) -> show t
                )
            $ xpSeq'
                (xpAttrFixed "task_state" "REGULAR")
                (xpTextAttr "stop_time")

        c =
            xpFilterAttr (hasName "task_state" <+> hasName "stop_time")
            $ xpWrap
                ( TaskStateCancel . parseUtcTime
                , \(TaskStateCancel t) -> show t
                )
            $ xpSeq'
                (xpAttrFixed "task_state" "CANCELLED")
                (xpTextAttr "stop_time")

        s =
            xpFilterAttr (hasName "task_state" <+> hasName "stop_time")
            $ xpWrap
                ( TaskStateStop . parseUtcTime
                , \(TaskStateStop t) -> show t
                )
            $ xpSeq'
                (xpAttrFixed "task_state" "STOPPED")
                (xpTextAttr "stop_time")

xpAwardedDistance :: PU (QTaskDistance Double [u| km |])
xpAwardedDistance =
    xpElem "FsFlightData"
    $ xpFilterAttr (hasName "distance")
    $ xpAttr "distance" xpickle

xpAwardedBonusDistance :: PU (QTaskDistance Double [u| km |])
xpAwardedBonusDistance =
    xpElem "FsFlightData"
    $ xpFilterAttr (hasName "bonus_distance")
    $ xpAttr "bonus_distance" xpickle

xpAwardedTime :: PU AwardedVelocity
xpAwardedTime =
    xpElem "FsFlightData"
    $ xpFilterAttr (hasName "started_ss" <+> hasName "finished_ss")
    $ xpWrap
        ( \(t0, tN) ->
                AwardedVelocity
                    (if null t0 then Nothing else Just $ parseUtcTime t0)
                    (if null tN then Nothing else Just $ parseUtcTime tN)
        , \AwardedVelocity{..} -> (maybe "" show ss, maybe "" show ss)
        )
    $ xpPair
        (xpTextAttr "started_ss")
        (xpTextAttr "finished_ss")

isGoalLine :: FsGoal -> GoalLine
isGoalLine (FsGoal "LINE") = GoalLine
isGoalLine _ = GoalNotLine

asAward
    :: String
    ->
        ( Pilot
        , Maybe (QTaskDistance Double [u| km |])
        , Maybe (QTaskDistance Double [u| km |])
        , Maybe AwardedVelocity
        )
    -> DfNoTrackPilot
asAward t' (p, d, bd, v) =
    DfNoTrackPilot
        { pilot = p

        , awardedReach = do
            ad' <- ad
            -- NOTE: The extra field is not a Maybe. It is the same as flown
            -- reach when there is no extra reach from bonus glide due to
            -- altitude above goal.
            ab' <- Just $ fromMaybe ad' ab
            return $ ReachToggle{flown = ad', extra = ab'}

        , awardedVelocity = fromMaybe (AwardedVelocity Nothing Nothing) v
        }
    where
        ad = asAwardReach t' d
        ab = asAwardReach t' bd


getTaskPilotGroup :: ArrowXml a => [Pilot] -> a XmlTree PilotGroup
getTaskPilotGroup ps =
    getChildren
    >>> deep (hasName "FsTask")
    >>> getTaskDistance
    &&& getAbsent
    &&& getDidNotFly
    &&& getDidFlyNoTracklog kps
    >>> arr mkGroup
    where
        mkGroup (t, (absentees, (dnf, nt))) =
            PilotGroup
                { absent = sort absentees
                , dnf = sort dnf
                , didFlyNoTracklog =
                    DfNoTrack
                    $ sortOn pilot
                    $ asAward t <$> nt
                }

        kps = keyPilots ps

        -- <FsParticipant id="82" />
        getAbsent =
            ( getChildren
            >>> hasName "FsParticipants"
            >>> listA getAbsentees
            )
            -- NOTE: If a task is created when there are no participants
            -- then the FsTask/FsParticipants element is omitted.
            `orElse` constA []
            where
                getAbsentees =
                    getChildren
                    >>> hasName "FsParticipant"
                        `notContaining` (getChildren >>> hasName "FsFlightData")
                    >>> getAttrValue "id"
                    >>> arr (unKeyPilot (keyMap kps) . PilotId)

        -- <FsParticipant id="80">
        --    <FsFlightData />
        getDidNotFly =
            ( getChildren
            >>> hasName "FsParticipants"
            >>> listA getDidNotFlyers
            )
            -- NOTE: If a task is created when there are no participants
            -- then the FsTask/FsParticipants element is omitted.
            `orElse` constA []
            where
                getDidNotFlyers =
                    getChildren
                    >>> hasName "FsParticipant"
                        `containing`
                        ( getChildren
                        >>> hasName "FsFlightData"
                        >>> neg (hasAttr "tracklog_filename"))
                    >>> getAttrValue "id"
                    >>> arr (unKeyPilot (keyMap kps) . PilotId)

        getTaskDistance =
            (getChildren
            >>> hasName "FsTaskScoreParams"
            >>> getAttrValue "task_distance")
            `orElse` constA ""

getDidFlyNoTracklog
    :: ArrowXml a
    => [KeyPilot]
    -> a
        _
        [
            ( Pilot
            , Maybe (QTaskDistance Double [u| km |])
            , Maybe (QTaskDistance Double [u| km |])
            , Maybe AwardedVelocity
            )
        ]
getDidFlyNoTracklog = getDidFlyIf (== "")

getDidFly
    :: ArrowXml a
    => [KeyPilot]
    -> a
        _
        [
            ( Pilot
            , Maybe (QTaskDistance Double [u| km |])
            , Maybe (QTaskDistance Double [u| km |])
            , Maybe AwardedVelocity
            )
        ]
getDidFly = getDidFlyIf (/= "")

-- <FsParticipant id="91">
--    <FsFlightData tracklog_filename="" />
-- <FsParticipant id="85">
--    <FsFlightData distance="95.030" tracklog_filename="" />
getDidFlyIf
    :: ArrowXml a
    => _
    -> [KeyPilot]
    -> a
        _
        [
            ( Pilot
            , Maybe (QTaskDistance Double [u| km |])
            , Maybe (QTaskDistance Double [u| km |])
            , Maybe AwardedVelocity
            )
        ]
getDidFlyIf predTrackLogFilename kps =
    getChildren
    >>> hasName "FsParticipants"
    >>> listA getDidFlyers
    -- NOTE: If a task is created when there are no participants
    -- then the FsTask/FsParticipants element is omitted.
    `orElse` constA []
    where
        getDidFlyers =
            getChildren
            >>> hasName "FsParticipant"
                `containing`
                ( getChildren
                >>> hasName "FsFlightData"
                >>> hasAttrValue "tracklog_filename" predTrackLogFilename)
            >>> getAttrValue "id"
            &&& getAwardedDistance
            &&& getAwardedBonusDistance
            &&& getAwardedTime
            >>> arr (\(pid, (ad, (abd, at))) -> (unKeyPilot (keyMap kps) . PilotId $ pid, ad, abd, at))

        -- WARNING: Expect distance="0.000" with tracklog_filename="".
        getAwardedDistance =
            (getChildren
            >>> hasName "FsFlightData"
            >>> arr (unpickleDoc xpAwardedDistance)
            )
            `orElse` constA Nothing

        getAwardedBonusDistance =
            (getChildren
            >>> hasName "FsFlightData"
            >>> arr (unpickleDoc xpAwardedBonusDistance)
            )
            `orElse` constA Nothing

        getAwardedTime =
            (getChildren
            >>> hasName "FsFlightData"
            >>> arr (unpickleDoc xpAwardedTime)
            )
            `orElse` constA Nothing


getTask
    :: ArrowXml a
    => Discipline
    -> Maybe Tweak
    -> Maybe (ScoreBackTime (Quantity Double [u| s |]))
    -> a XmlTree (Task k)
getTask discipline compTweak sb =
    getChildren
    >>> deep (hasName "FsTask")
    >>> getAttrValue "name"
    &&& ((getGoal >>> arr isGoalLine)
        &&& getDecelerator
        &&& getHeading
        &&& getSpeedSection
        &&& getZoneAltitudes
        &&& getZones
        >>> arr (\(a, (b, (c, (d, (e, f))))) -> mkZones discipline a b c d e f)
        )
    &&& getSpeedSection
    &&& getZoneTimes
    &&& getStartGates
    &&& getTaskState
    &&& getTaskTweak
    &&& getEarlyStart
    >>> arr mkTask
    where
        mkTask (name, (zs, (section, (ts, (gates, (taskState, (tw, es))))))) =
            Task
                { taskName = name
                , zones = zs
                , speedSection = section
                , zoneTimes = ts''
                , startGates = gates
                , stopped =
                    -- NOTE: In paragliding there is a score back time but in
                    -- hang gliding this is taken as the interval between the
                    -- start gates or 15 mins.
                    maybe
                        Nothing
                        (\case
                            TaskStateStop t ->
                                Just $ TaskStop
                                    { announced = t
                                    , retroactive =
                                        case discipline of
                                            HangGliding ->
                                                let diff =
                                                        case gates of
                                                            StartGate g0 : StartGate g1 : _ -> g0 `diffUTCTime` g1
                                                            _ -> realToFrac $ negate (900 :: Int)

                                                in diff `addUTCTime` t

                                            Paragliding ->
                                                maybe
                                                    t
                                                    (\(ScoreBackTime (MkQuantity secs)) ->
                                                        realToFrac (negate secs) `addUTCTime` t)
                                                    sb
                                    }

                            TaskStateRegular _ -> Nothing
                            TaskStateCancel _ -> Nothing)
                        taskState
                , cancelled =
                    maybe
                        False
                        (\case
                            TaskStateStop{} -> False
                            TaskStateRegular _ -> False
                            TaskStateCancel _ -> True)
                        taskState
                , taskTweak = if tw == compTweak then compTweak else tw
                , earlyStart = fromMaybe nullEarlyStart es
                , penalsAuto = []
                , penals = []
                }
            where
                -- NOTE: If all time zones are the same then collapse.
                ts' = nub ts
                ts'' = if length ts' == 1 then ts' else ts

        getHeading =
            (getChildren
            >>> hasName "FsTaskDefinition"
            /> hasName "FsHeadingpoint"
            >>> arr (unpickleDoc xpHeading)
            )
            `orElse` constA Nothing

        getEarlyStart =
            if discipline == Paragliding then constA Nothing else
            getChildren
            >>> hasName "FsScoreFormula"
            >>> arr (unpickleDoc xpEarlyStart)

        getDecelerator =
            if discipline == HangGliding then constA Nothing else
            getChildren
            >>> hasName "FsScoreFormula"
            >>> arr (unpickleDoc xpDecelerator)

        getGoal =
            getChildren
            >>> hasName "FsTaskDefinition"
            >>> getAttrValue "goal"
            >>> arr FsGoal

        getZoneAltitudes =
            getChildren
            >>> hasName "FsTaskDefinition"
            >>> listA getAlts
            where
                getAlts =
                    getChildren
                    >>> hasName "FsTurnpoint"
                    -- NOTE: Sometimes turnpoints don't have altitudes.
                    >>> arr (unpickleDoc xpZoneAltitude)

        getZones =
            getChildren
            >>> hasName "FsTaskDefinition"
            >>> (listA getTps >>> arr catMaybes)
            where
                getTps =
                    getChildren
                    >>> hasName "FsTurnpoint"
                    >>> arr (unpickleDoc xpZone)

        getZoneTimes =
            getChildren
            >>> hasName "FsTaskDefinition"
            >>> (listA getOpenClose >>> arr catMaybes)
            where
                getOpenClose =
                    getChildren
                    >>> hasName "FsTurnpoint"
                    >>> arr (unpickleDoc xpOpenClose)

        getStartGates =
            getChildren
            >>> hasName "FsTaskDefinition"
            >>> (listA getGates >>> arr catMaybes)
            where
                getGates =
                    getChildren
                    >>> hasName "FsStartGate"
                    >>> arr (unpickleDoc xpStartGate)

        getSpeedSection =
            getChildren
            >>> hasName "FsTaskDefinition"
            >>> arr (unpickleDoc xpSpeedSection)

        getTaskState =
            getChildren
            >>> hasName "FsTaskState"
            >>> arr (unpickleDoc xpStopped)

        getTaskTweak =
            getChildren
            >>> hasName "FsScoreFormula"
            >>> arr (unpickleDoc $ xpTweak discipline)

getTaskPilotPenaltiesAuto
    :: ArrowXml a
    => [Pilot]
    -> a XmlTree [(Pilot, PenaltySeq, String)]
getTaskPilotPenaltiesAuto pilots =
    getChildren
    >>> deep (hasName "FsTask")
    >>> getPenalty
    where
        kps = keyPilots pilots

        -- <FsParticipant id="28">
        --    <FsResultPenalty penalty="0" penalty_points="-253" penalty_reason="" />
        getPenalty =
            ( getChildren
            >>> hasName "FsParticipants"
            >>> listA getDidIncur
            )
            -- NOTE: If a task is created when there are no participants
            -- then the FsTask/FsParticipants element is omitted.
            `orElse` constA []
            where
                getDidIncur =
                    getChildren
                    >>> hasName "FsParticipant"
                        `containing`
                        ( getChildren
                        >>> hasName "FsResult"
                        >>> hasAttr "penalty_points_auto"
                        >>> hasAttr "penalty_reason_auto")
                        `notContaining`
                        hasAttrValue "penalty_points_auto" (== "0")
                    >>> getAttrValue "id"
                    &&& getResultPenaltyAuto
                    >>> arr (\(pid, x) ->
                        let (ps, s) = fromMaybe (idSeq, "") x in
                        (unKeyPilot (keyMap kps) . PilotId $ pid, ps, s))

                getResultPenaltyAuto =
                    getChildren
                    >>> hasName "FsResult"
                    >>> arr (unpickleDoc xpPointPenaltyAuto)

getTaskPilotPenalties
    :: ArrowXml a
    => [Pilot]
    -> a XmlTree [(Pilot, PenaltySeq, String)]
getTaskPilotPenalties pilots =
    getChildren
    >>> deep (hasName "FsTask")
    >>> getPenalty
    where
        kps = keyPilots pilots

        -- <FsParticipant id="28">
        --    <FsResultPenalty penalty="0" penalty_points="-253" penalty_reason="" />
        getPenalty =
            ( getChildren
            >>> hasName "FsParticipants"
            >>> listA getDidIncur
            )
            -- NOTE: If a task is created when there are no participants
            -- then the FsTask/FsParticipants element is omitted.
            `orElse` constA []
            where
                getDidIncur =
                    getChildren
                    >>> hasName "FsParticipant"
                        `containing`
                        (( getChildren
                        >>> hasName "FsResultPenalty"
                        >>> hasAttr "penalty"
                        >>> hasAttr "penalty_points"
                        >>> hasAttr "penalty_reason")
                        -- NOTE: (>>>) is logical and (<+>) is logical or.
                        -- "A logical and can be formed by a1 >>> a2 , a locical or by a1 <+> a2"
                        -- SEE: http://hackage.haskell.org/package/hxt-9.3.1.16/docs/Text-XML-HXT-Arrow-XmlArrow.html
                        -- NOTE: Avoid penalties with no non-zero value.
                        -- <FsParticipant id="88">
                        --   <FsResultPenalty penalty="0" penalty_points="0" penalty_reason="" />
                        `notContaining`
                        ( hasAttrValue "penalty" (== "0")
                        >>> hasAttrValue "penalty_points" (== "0")
                        ))
                    >>> getAttrValue "id"
                    &&& getResultPenalty
                    >>> arr (\(pid, x) ->
                        let (ps, s) = fromMaybe (idSeq, "") x in
                        (unKeyPilot (keyMap kps) . PilotId $ pid, ps, s))

                getResultPenalty =
                    getChildren
                    >>> hasName "FsResultPenalty"
                    >>> arr (unpickleDoc xpPointPenalty)

parseTasks
    :: Discipline
    -> Maybe Tweak
    -> Maybe (ScoreBackTime (Quantity Double [u| s |]))
    -> String
    -> IO (Either String [Task k])
parseTasks discipline compTweak sb contents = do
    let doc = readString [ withValidate no, withWarnings no ] contents
    xs <- runX $ doc >>> getTask discipline compTweak sb
    return $ Right xs

parseTaskPilotGroups :: String -> IO (Either String [PilotGroup])
parseTaskPilotGroups contents = do
    let doc = readString [ withValidate no, withWarnings no ] contents
    ps <- runX $ doc >>> getCompPilot
    xs <- runX $ doc >>> getTaskPilotGroup ps
    return $ Right xs

parseTaskPilotPenaltiesAuto
    :: String
    -> IO (Either String [[(Pilot, PenaltySeq, String)]])
parseTaskPilotPenaltiesAuto contents = do
    let doc = readString [ withValidate no, withWarnings no ] contents
    ps <- runX $ doc >>> getCompPilot
    xs <- runX $ doc >>> getTaskPilotPenaltiesAuto ps
    return $ Right xs

parseTaskPilotPenalties
    :: String
    -> IO (Either String [[(Pilot, PenaltySeq, String)]])
parseTaskPilotPenalties contents = do
    let doc = readString [ withValidate no, withWarnings no ] contents
    ps <- runX $ doc >>> getCompPilot
    xs <- runX $ doc >>> getTaskPilotPenalties ps
    return $ Right xs
