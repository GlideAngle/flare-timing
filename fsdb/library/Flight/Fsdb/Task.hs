{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.Fsdb.Task (parseTasks) where

import Data.Maybe (catMaybes)
import Data.List (sort, nub)
import Data.Map.Strict (Map, fromList, findWithDefault)
import Data.UnitsOfMeasure (u, convert, fromRational')
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Text.XML.HXT.Arrow.Pickle
    ( XmlPickler(..), PU(..)
    , xpickle, unpickleDoc, xpWrap, xpFilterAttr, xpElem, xpAttr, xpAlt
    , xpText, xpPair, xpTriple, xp4Tuple, xpInt, xpTrees, xpAttrFixed, xpSeq'
    , xpPrim
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
    , constA
    , listA
    , arr
    , deep
    , notContaining
    , orElse
    )
import Data.Time.Clock (UTCTime)
import Data.Time.Format (parseTimeOrError, defaultTimeLocale)

import Flight.Units ()
import Flight.LatLng (LatLng(..), Lat(..), Lng(..), Alt(..), QAlt)
import Flight.LatLng.Raw (RawLat(..), RawLng(..))
import Flight.Zone (Radius(..), AltTime(..))
import Flight.Zone.MkZones
    ( Discipline(..), Decelerator(..), CessIncline(..)
    , DeceleratorShape(..), EssVsGoal(..), GoalLine(..)
    , tpKindShape, essKindShape, goalKindShape
    , mkTpKind, mkEssKind, mkGoalKind
    )
import Flight.Zone.TaskZones (ToZoneKind, raceZoneKinds, openZoneKinds)
import Flight.Zone.ZoneKind (ZoneKind(..), Goal, OpenDistance)
import qualified Flight.Zone.Raw as Z (RawZone(..))
import Flight.Comp
    ( PilotId(..), PilotName(..), Pilot(..), SpeedSection
    , Task(..), TaskStop(..), Zones(..), StartGate(..), OpenClose(..)
    )
import Flight.Fsdb.Pilot (getCompPilot)
import Flight.Fsdb.Internal.XmlPickle (xpNewtypeRational, xpNewtypeQuantity)

-- | The attribute //FsTaskDefinition@goal.
newtype FsGoal = FsGoal String

newtype KeyPilot = KeyPilot (PilotId, Pilot)

instance (u ~ Quantity Double [u| m |]) => XmlPickler (Radius u) where
    xpickle = xpNewtypeQuantity

keyMap :: [KeyPilot] -> Map PilotId Pilot
keyMap = fromList . fmap (\(KeyPilot x) -> x)
                        
unKeyPilot :: Map PilotId Pilot -> PilotId -> Pilot
unKeyPilot ps k@(PilotId ip) =
    findWithDefault (Pilot (k, PilotName ip)) k ps

instance XmlPickler RawLat where
    xpickle = xpNewtypeRational

instance XmlPickler RawLng where
    xpickle = xpNewtypeRational

instance (u ~ Quantity Double [u| m |]) => XmlPickler (Alt u) where
    xpickle = xpNewtypeQuantity

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
                (xpAttr "no_final_glide_decelerator_reason" xpText)

xpZone :: PU Z.RawZone
xpZone =
    xpElem "FsTurnpoint"
    $ xpFilterAttr
        ( hasName "id"
        <+> hasName "lat"
        <+> hasName "lon"
        <+> hasName "radius"
        )
    $ xpWrap
        ( \(n, lat, lng, r) -> Z.RawZone n lat lng r
        , \Z.RawZone{..} -> (zoneName, lat, lng, radius)
        )
    $ xp4Tuple
        (xpAttr "id" xpText)
        (xpAttr "lat" xpickle)
        (xpAttr "lon" xpickle)
        (xpAttr "radius" xpickle)

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
        (xpAttr "open" xpText)
        (xpAttr "close" xpText)

xpStartGate :: PU StartGate
xpStartGate =
    xpElem "FsStartGate"
    $ xpFilterAttr (hasName "open")
    $ xpWrap
        ( StartGate . parseUtcTime
        , \(StartGate open) -> show open
        )
    $ xpAttr "open" xpText

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

xpStopped :: PU TaskStop
xpStopped =
    xpElem "FsTaskState"
    $ xpFilterAttr (hasName "task_state" <+> hasName "stop_time")
    $ xpWrap
        ( TaskStop . parseUtcTime
        , \(TaskStop t) -> show t
        )
    $ xpSeq'
        (xpAttrFixed "task_state" "STOPPED")
        (xpAttr "stop_time" xpText)

mkZones
    :: Discipline
    -> ( GoalLine
       ,
           ( Maybe Decelerator
           ,
               ( Maybe (LatLng Rational [u| deg |])
               ,
                   ( SpeedSection
                   ,
                       ( [Maybe (QAlt Double [u| m |])]
                       , [Z.RawZone]
                       )
                   )
               )
           )
       )
    -> Zones

mkZones _ (_, (_, (heading, (Nothing, (alts, zs))))) =
    Zones zs Nothing (g alts zs)
    where
        zsLen = length zs
        psLen = 0
        tsLen = zsLen - psLen - 1

        tk r x _ = Cylinder r x

        okCyl :: ToZoneKind OpenDistance
        okCyl r x _ = Star r x

        okVec :: LatLng Rational [u| deg |] -> ToZoneKind OpenDistance
        okVec (LatLng (Lat dLat, Lng dLng)) =
            let rLat :: Quantity _ [u| rad |]
                rLat = fromRational' . convert $ dLat

                rLng :: Quantity _ [u| rad |]
                rLng = fromRational' . convert $ dLng

                y = LatLng (Lat rLat, Lng rLng)

            in \r x _ -> Vector (Left y) r x

        ok = maybe okCyl okVec heading

        ps = replicate psLen tk
        ts = replicate tsLen tk
        g = openZoneKinds ps ts ok

mkZones discipline (goalLine, (decel, (_, (speed@(Just _), (alts, zs))))) =
    Zones zs (g alts zs) Nothing
    where
        -- The number of zones.
        zsLen = length zs

        -- The number of prolog zones.
        psLen = maybe 0 ((\x -> x - 1) . fst) speed

        -- The number of epilog zones.
        esLen = maybe 0 ((\x -> max 0 $ zsLen - x - 1) . snd) speed

        -- The 1-based index of the end of the speed section.
        ssEnd = maybe zsLen snd speed

        -- The remaining turnpoint zones in the race excluding the zone at the
        -- end of the speed section. That race end zone might be either goal
        -- (when ssEnd == zsLen) or an ESS zone.
        tsLen = zsLen - psLen - esLen - (if ssEnd == zsLen then 1 else 2)

        tpShape = tpKindShape discipline goalLine

        dcShape =
            \case
                CESS 0 -> DecCyl
                CESS _ -> DecCone
                _ -> DecCyl
            <$> decel

        ssEndIsGoal = if ssEnd == zsLen then EssAtGoal else EssBeforeGoal
        gkShape = goalKindShape discipline goalLine ssEndIsGoal dcShape
        ekShape = essKindShape discipline goalLine ssEndIsGoal dcShape 

        gk :: ToZoneKind Goal
        gk = mkGoalKind gkShape decel

        tk = mkTpKind tpShape
        ek = if ssEnd < zsLen then Just $ mkEssKind ekShape decel else Nothing

        ps = replicate psLen tk
        ts = replicate tsLen tk
        es = replicate esLen tk
        g = raceZoneKinds ps ts ek es gk

isGoalLine :: FsGoal -> GoalLine
isGoalLine (FsGoal "LINE") = GoalLine
isGoalLine _ = GoalNotLine

getTask :: ArrowXml a => Discipline -> [Pilot] -> a XmlTree (Task k)
getTask discipline ps =
    getChildren
    >>> deep (hasName "FsTask")
    >>> getAttrValue "name"
    &&& ((getGoal >>> arr isGoalLine)
        &&& getDecelerator
        &&& getHeading
        &&& getSpeedSection
        &&& getZoneAltitudes
        &&& getZones
        >>> arr (mkZones discipline)
        )
    &&& getSpeedSection
    &&& getZoneTimes
    &&& getStartGates
    &&& getAbsent
    &&& getStopped
    >>> arr mkTask
    where
        kps = (\x@(Pilot (k, _)) -> KeyPilot (k, x)) <$> ps
        
        getHeading =
            (getChildren
            >>> hasName "FsTaskDefinition"
            /> hasName "FsHeadingpoint"
            >>> arr (unpickleDoc xpHeading)
            )
            `orElse` constA Nothing

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

        getStopped =
            getChildren
            >>> hasName "FsTaskState"
            >>> arr (unpickleDoc xpStopped)

        mkTask (name, (zs, (section, (ts, (gates, (absentees, stop)))))) =
            Task name zs section ts'' gates (sort absentees) stop
            where
                -- NOTE: If all time zones are the same then collapse.
                ts' = nub ts
                ts'' = if length ts' == 1 then ts' else ts

parseTasks :: Discipline -> String -> IO (Either String [Task k])
parseTasks discipline contents = do
    let doc = readString [ withValidate no, withWarnings no ] contents
    ps <- runX $ doc >>> getCompPilot
    xs <- runX $ doc >>> getTask discipline ps
    return $ Right xs

parseUtcTime :: String -> UTCTime
parseUtcTime =
    -- NOTE: %F is %Y-%m-%d, %T is %H:%M:%S and %z is -HHMM or -HH:MM
    parseTimeOrError False defaultTimeLocale "%FT%T%Z"

