{-# OPTIONS_GHC -fno-warn-orphans #-}

module Flight.Fsdb.Task (parseTasks) where

import Data.Maybe (catMaybes)
import Data.List (sort, nub)
import Data.Map.Strict (Map, fromList, findWithDefault)
import Data.UnitsOfMeasure (u)
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

import Flight.LatLng (Alt(..), QAlt)
import Flight.LatLng.Raw (RawLat(..), RawLng(..))
import Flight.Zone
    (Radius(..), Zone(..), AltTime(..), QIncline, Incline(..), RawZoneToZone
    , rawZonesToZones
    )
import Flight.Zone.ZoneKind (RawZoneToZoneKind, rawZonesToZoneKinds)
import qualified Flight.Zone.ZoneKind as ZK
    ( ZoneKind(..), Turnpoint, EndOfSpeedSection, Goal
    , EssAllowedZone, GoalAllowedZone
    )
import qualified Flight.Zone.Raw as Z (RawZone(..))
import Flight.Comp
    ( PilotId(..), PilotName(..), Pilot(..)
    , Task(..), TaskStop(..), Zones(..), StartGate(..), OpenClose(..)
    )
import Flight.Fsdb.Pilot (getCompPilot)
import Flight.Units ()
import Flight.Score (Discipline(..))
import Flight.Fsdb.Internal.XmlPickle (xpNewtypeRational, xpNewtypeQuantity)

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

data Decelerator
    = CESS Double
    | AATB Double
    | NoDecelerator String
    deriving Show

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
            $ xpWrap (CESS, \(CESS r) -> r)
            $ xpSeq'
                (xpAttrFixed "final_glide_decelerator" "cess")
                (xpAttr "ess_incline_ratio" xpPrim)

        a =
            xpFilterAttr
                ( hasName "final_glide_decelerator"
                <+> hasName "aatb_factor"
                )
            $ xpWrap (AATB, \(AATB r) -> r)
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
        , \(Z.RawZone{..}) -> (zoneName, lat, lng, radius)
        )
    $ xp4Tuple
        (xpAttr "id" xpText)
        (xpAttr "lat" xpickle)
        (xpAttr "lon" xpickle)
        (xpAttr "radius" xpickle)

xpZoneAltitudes :: PU (QAlt Double [u| m |])
xpZoneAltitudes =
    xpElem "FsTurnpoint"
    $ xpFilterAttr (hasName "altitude")
    $ xpAttr "altitude" xpickle

xpOpenClose :: PU OpenClose
xpOpenClose =
    xpElem "FsTurnpoint"
    $ xpFilterAttr (hasName "open" <+> hasName "close")
    $ xpWrap
        ( \(open, close) -> OpenClose (parseUtcTime open) (parseUtcTime close)
        , \(OpenClose{..}) -> (show open, show close)
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
        ( (\(a, b, _) -> (a, b))
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
    -> ( Maybe Decelerator
       ,
           ( Bool
           ,
               ( FsGoal
               ,
                   ( [QAlt Double [u| m |]]
                   , [Z.RawZone]
                   )
               )
           )
       )
    -> Zones
mkZones discipline (decel, (useSemi, (goal, (alts, zs)))) =
    Zones zs (f zs) (g alts zs)
    where
        f = rawZonesToZones $ mkGoal discipline useSemi goal

        gk :: RawZoneToZoneKind ZK.Goal
        gk = mkGoalKind discipline decel useSemi goal

        tk = mkTpKind discipline useSemi goal
        ek = mkEssKind discipline decel useSemi goal
        ts = (replicate ((length alts) - 1) tk)
        es = []
        g = rawZonesToZoneKinds ts ek es gk

-- | The attribute //FsTaskDefinition@goal.
newtype FsGoal = FsGoal String

getTask :: ArrowXml a => Discipline -> [Pilot] -> a XmlTree (Task k)
getTask discipline ps =
    getChildren
    >>> deep (hasName "FsTask")
    >>> getAttrValue "name"
    &&& ( getDecelerator
        &&& getFormula
        &&& getGoal
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
        
        getDecelerator =
            if discipline == HangGliding then constA Nothing else
            getChildren
            >>> hasName "FsScoreFormula"
            >>> arr (unpickleDoc xpDecelerator)

        getFormula =
            getChildren
            >>> hasName "FsScoreFormula"
            >>> getAttrValue "use_semi_circle_control_zone_for_goal_line"
            >>> arr (== "1")

        getGoal =
            getChildren
            >>> hasName "FsTaskDefinition"
            >>> getAttrValue "goal"
            >>> arr FsGoal

        getZoneAltitudes =
            getChildren
            >>> hasName "FsTaskDefinition"
            >>> (listA getAlts >>> arr catMaybes)
            where
                getAlts =
                    getChildren
                    >>> hasName "FsTurnpoint"
                    >>> arr (unpickleDoc xpZoneAltitudes)

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
            `orElse` (constA [])
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

mkGoal :: Discipline -> Bool -> FsGoal -> RawZoneToZone
mkGoal Paragliding True (FsGoal "LINE") = SemiCircle
mkGoal _ _ (FsGoal "LINE") = Line
mkGoal _ _ _ = Circle

mkGoalKind
    :: (ZK.EssAllowedZone k, ZK.GoalAllowedZone k)
    => Discipline
    -> Maybe Decelerator
    -> Bool
    -> FsGoal
    -> RawZoneToZoneKind k
mkGoalKind Paragliding (Just (CESS r)) _ _ = ZK.CutCone $ mkIncline r
mkGoalKind Paragliding (Just (AATB r)) _ _ = ZK.CutCylinder (AltTime $ MkQuantity r)
mkGoalKind Paragliding _ True (FsGoal "LINE") = \r x _ -> ZK.SemiCircle r x
mkGoalKind _ _ _ (FsGoal "LINE") = \r x _ -> ZK.Line r x
mkGoalKind _ _ _ _ = \r x _ -> ZK.Circle r x

mkTpKind
    :: Discipline
    -> Bool
    -> FsGoal
    -> RawZoneToZoneKind ZK.Turnpoint
mkTpKind _ _ _ = \r x _ -> ZK.Cylinder r x

mkEssKind
    :: Discipline
    -> Maybe Decelerator
    -> Bool
    -> FsGoal
    -> RawZoneToZoneKind ZK.EndOfSpeedSection
mkEssKind Paragliding (Just (CESS r)) _ _ = ZK.CutCone $ mkIncline r
mkEssKind Paragliding (Just (AATB r)) _ _ = ZK.CutCylinder (AltTime $ MkQuantity r)
mkEssKind _ _ _ _ = \r x _ -> ZK.Cylinder r x

mkIncline :: Double -> QIncline Double [u| rad |]
mkIncline r =
    Incline $ MkQuantity . atan $ r
