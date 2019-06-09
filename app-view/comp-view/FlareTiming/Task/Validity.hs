module FlareTiming.Task.Validity (viewValidity) where

import Prelude hiding (sum)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (TimeZone)
import Reflex
import Reflex.Dom
import Data.String (IsString)
import Text.Printf (printf)
import qualified Data.Text as T (Text, pack)
import Data.List (partition)
import Data.Map (Map)
import qualified Data.Map.Strict as Map

import qualified WireTypes.Validity as Vy
    ( Validity(..)
    , showTaskValidity, showTaskValidityDiff
    , showLaunchValidity, showLaunchValidityDiff
    , showDistanceValidity, showDistanceValidityDiff
    , showTimeValidity, showTimeValidityDiff
    , showStopValidity, showStopValidityDiff
    )
import WireTypes.ValidityWorking
    ( ValidityWorking(..)
    , LaunchValidityWorking(..)
    , DistanceValidityWorking(..)
    , TimeValidityWorking(..)
    , BestTime(..)
    , PilotsFlying(..)
    , MaximumDistance(..)
    )
import WireTypes.Cross (FlyingSection)
import WireTypes.Route (TaskDistance(..), showTaskDistance)
import WireTypes.Reach (TrackReach(..), ReachStats(..))
import WireTypes.Point (PilotDistance(..), showPilotDistance)
import WireTypes.Pilot (Pilot(..))
import WireTypes.Comp (Task(..), UtcOffset(..), TaskStop(..))
import FlareTiming.Pilot (showPilotName)
import FlareTiming.Time (timeZone, showTime)

katexNewLine :: T.Text
katexNewLine = " \\\\\\\\ "

hookWorking
    :: Vy.Validity
    -> ValidityWorking
    -> ReachStats
    -> TaskDistance
    -> Int
    -> T.Text
hookWorking v ValidityWorking{launch = l, distance = d, time = t} r td landed =
    taskWorking v
    <> launchWorking v l
    <> distanceWorking v d
    <> timeWorking v t
    <> stopWorking d t r td landed

taskWorking :: Vy.Validity -> T.Text
taskWorking v =
    "katex.render("
    <> "\"\\\\begin{aligned} "
    <> "validity &= lv * dv * tv"
    <> (maybe "" (const " * sv") (Vy.stop v))
    <> katexNewLine
    <> " &= "
    <> (Vy.showLaunchValidity . Vy.launch $ v)
    <> " * "
    <> (Vy.showDistanceValidity . Vy.distance $ v)
    <> " * "
    <> (Vy.showTimeValidity . Vy.time $ v)
    <> (maybe "" (\sv -> " * " <> (Vy.showStopValidity $ Just sv)) (Vy.stop v))
    <> katexNewLine
    <> " &= "
    <> (Vy.showTaskValidity . Vy.task $ v)
    <> " \\\\end{aligned}\""
    <> ", getElementById('task-working')"
    <> ", {throwOnError: false});"

launchWorking :: Vy.Validity -> LaunchValidityWorking -> T.Text
launchWorking v w@LaunchValidityWorking{flying = f} =
    "katex.render("
    <> "\"\\\\begin{aligned} "
    <> "x &= \\\\min(1, \\\\frac{f}{p * n})"
    <> katexNewLine
    <> " &= \\\\min(1, \\\\frac{"
    <> (T.pack . show $ f)
    <> "}{"
    <> (T.pack . show $ present w)
    <> " * "
    <> (T.pack . show $ nominalLaunch w)
    <> "})"
    <> katexNewLine
    <> katexNewLine
    <> "validity &= 0.027 * x + 2.917 * x^2 - 1.944 * x^3"
    <> katexNewLine
    <> " &= "
    <> (Vy.showLaunchValidity . Vy.launch $ v)
    <> " \\\\end{aligned}\""
    <> ", getElementById('launch-working')"
    <> ", {throwOnError: false});"

distanceWorkingSubA :: DistanceValidityWorking -> T.Text
distanceWorkingSubA
    DistanceValidityWorking
        { nominalGoal = ng
        , nominalDistance = nd
        , minimumDistance = md
        } =
    " &= ("
    <> (T.pack . show $ ng)
    <> " + 1) * ("
    <> (T.pack . show $ nd)
    <> " - "
    <> (T.pack . show $ md)
    <> ")"

distanceWorkingSubB :: DistanceValidityWorking -> T.Text
distanceWorkingSubB
    DistanceValidityWorking
        { nominalGoal = ng
        , bestDistance = bd
        , nominalDistance = nd
        } =
    " &="
    <> " \\\\max(0, "
    <> (T.pack . show $ ng)
    <> " * ("
    <> (T.pack . show $ bd)
    <> " - "
    <> (T.pack . show $ nd)
    <> ")"

distanceWorking :: Vy.Validity -> DistanceValidityWorking -> T.Text
distanceWorking v w =
    "katex.render("
    <> "\"\\\\begin{aligned} "
    <> " sum"
    <> " &="
    <> " \\\\sum_p \\\\max(0, d_p - md)"
    <> " &\\\\"
    <> " d_p = \\\\text{distance flown by pilot \\\\textit{p}}"
    <> katexNewLine
    <> " &= "
    <> (T.pack . show $ sum w)
    <> katexNewLine
    <> katexNewLine
    <> " a &= (ng + 1) * (nd - md)"
    <> katexNewLine
    <> distanceWorkingSubA w
    <> katexNewLine
    <> katexNewLine
    <> " b &="
    <> " \\\\max(0, ng * (bd - nd)"
    <> katexNewLine
    <> distanceWorkingSubB w
    <> katexNewLine
    <> katexNewLine
    <> "area"
    <> " &="
    <> "\\\\frac{(a + b)}{2}"
    <> katexNewLine
    <> " &= "
    <> (T.pack . show $ area w)
    <> katexNewLine
    <> katexNewLine
    <> " validity &="
    <> " \\\\min(1, \\\\frac{sum}{f * area})"
    <> katexNewLine
    <> " &= "
    <> (Vy.showDistanceValidity . Vy.distance $ v)
    <> " \\\\end{aligned}\""
    <> ", getElementById('distance-working')"
    <> ", {throwOnError: false});"

timeWorkingCase :: (Semigroup p, IsString p) => Maybe a -> p
timeWorkingCase (Just _) = " &= \\\\dfrac{bt}{nt}"
timeWorkingCase Nothing = " &= \\\\dfrac{bd}{nd}"

timeWorkingSub :: TimeValidityWorking -> T.Text

timeWorkingSub
    TimeValidityWorking{gsBestTime = Just bt@(BestTime b), nominalTime = nt} =
    " &="
    <> " \\\\dfrac{"
    <> (T.pack . show $ bt)
    <> "}{"
    <> (T.pack . show $ nt)
    <> "}"
    <> katexNewLine
    <> " &="
    <> " \\\\dfrac{"
    <> (T.pack . printf "%f h" $ b)
    <> "}{"
    <> (T.pack . show $ nt)
    <> "}"

timeWorkingSub
    TimeValidityWorking{bestDistance = bd, nominalDistance = nd} =
    " &="
    <> " \\\\dfrac{"
    <> (T.pack . show $ bd)
    <> "}{"
    <> (T.pack . show $ nd)
    <> "}"

timeWorking :: Vy.Validity -> TimeValidityWorking -> T.Text
timeWorking v w@TimeValidityWorking{gsBestTime = bt} =
    "katex.render("
    <> "\"\\\\begin{aligned} "
    <> " x &="
    <> " \\\\begin{cases}"
    <> " \\\\dfrac{bt}{nt}"
    <> " &\\\\text{if at least one pilot reached ESS}"
    <> katexNewLine
    <> katexNewLine
    <> " \\\\dfrac{bd}{nd}"
    <> " &\\\\text{if no pilots reached ESS}"
    <> " \\\\end{cases}"
    <> katexNewLine
    <> timeWorkingCase bt
    <> katexNewLine
    <> timeWorkingSub w
    <> katexNewLine
    <> katexNewLine
    <> " y &= \\\\min(1, x)"
    <> katexNewLine
    <> katexNewLine
    <> " z &= -0.271 + 2.912 * y - 2.098 * y^2 + 0.457 * y^3"
    <> katexNewLine
    <> katexNewLine
    <> "validity &= \\\\max(0, \\\\min(1, z))"
    <> katexNewLine
    <> " &= "
    <> (Vy.showTimeValidity . Vy.time $ v)
    <> " \\\\end{aligned}\""
    <> ", getElementById('time-working')"
    <> ", {throwOnError: false});"


stopWorkingCase :: Maybe a -> Double -> Double -> (Double, T.Text)
stopWorkingCase (Just _) _ _ = (1, " &= 1")
stopWorkingCase Nothing a b = (min 1 (a + b3), eqn) where
        eqn =
            " &= \\\\min(1, a + b^3)"
            <> katexNewLine
            <> " &= \\\\min(1, " <> a' <> " + " <> b' <> ")"

        b3 = b**3
        b' = T.pack $ printf "%.3f" b3
        a' = T.pack $ printf "%.3f" a

stopWorkingSubA :: DistanceValidityWorking -> ReachStats -> TaskDistance -> (Double, T.Text)

stopWorkingSubA
    DistanceValidityWorking{bestDistance = bd@(MaximumDistance bd')}
    ReachStats
        { flownMean = mf@(PilotDistance mf')
        , flownStdDev = sf@(PilotDistance sf')
        }
    td@(TaskDistance td') = (z, eqn) where
        eqn =
            " &= \\\\sqrt{\\\\frac{"
            <> bd''
            <> " - "
            <> mf''
            <> "}{"
            <> ed
            <> " - "
            <> bd''
            <> " + 1} * \\\\sqrt{\\\\frac{"
            <> sf''
            <> "}{5}}}"
            <> katexNewLine
            <> " &= \\\\sqrt{\\\\frac{"
            <> textf (bd' - mf')
            <> "}{"
            <> textf (td' - bd' + 1)
            <> "} * \\\\sqrt{"
            <> textf (sf' / 5)
            <> "}}"
            <> katexNewLine
            <> " &= \\\\sqrt{"
            <> x'
            <> " * "
            <> y'
            <> "}"
            <> katexNewLine
            <> (" &= " <> z')

        bd'' = T.pack $ show bd
        ed = showTaskDistance td
        mf'' = showPilotDistance 3 mf <> "km"
        sf'' = showPilotDistance 3 sf <> "km"

        textf = T.pack . printf "%.3f"
        x' = textf x
        y' = textf y
        z' = textf z

        x = (bd' - mf') / (td' - bd' + 1)
        y = sqrt $ sf' / 5
        z = sqrt $ x * y

stopWorkingSubB :: DistanceValidityWorking -> Int -> Double -> T.Text

stopWorkingSubB DistanceValidityWorking{flying = PilotsFlying pf} landed b' =
    " &= \\\\frac{"
    <> ls
    <> "}{"
    <> f
    <> "}"
    <> katexNewLine
    <> (" &= " <> b)
    where
        ls = T.pack . show $ landed
        f = T.pack . show $ pf
        b = T.pack $ printf "%.3f" b'

stopWorking
    :: DistanceValidityWorking
    -> TimeValidityWorking
    -> ReachStats
    -> TaskDistance
    -> Int
    -> T.Text
stopWorking
    dw@DistanceValidityWorking{flying = PilotsFlying pf}
    TimeValidityWorking{gsBestTime = bt}
    reachStats
    td
    landed =

    "katex.render("
    <> "\"\\\\begin{aligned} "
    <> " a &= \\\\sqrt{\\\\frac{bd - \\\\overline{flown}}{ed - bd + 1} * \\\\sqrt{\\\\frac{\\\\sigma(flown)}{5}}}"
    <> katexNewLine
    <> eqnA
    <> katexNewLine
    <> katexNewLine
    <> " b &= \\\\frac{ls}{f}"
    <> katexNewLine
    <> stopWorkingSubB dw landed b
    <> katexNewLine
    <> katexNewLine
    <> " validity &="
    <> " \\\\begin{cases}"
    <> " 1"
    <> " &\\\\text{if at least one pilot reached ESS}"
    <> katexNewLine
    <> katexNewLine
    <> " \\\\min(1, a + b^3)"
    <> " &\\\\text{if no pilots reached ESS}"
    <> " \\\\end{cases}"
    <> katexNewLine
    <> eqnV
    <> katexNewLine
    <> (" &= " <> (T.pack $ printf "%.3f" v))
    <> " \\\\end{aligned}\""
    <> ", getElementById('stop-working')"
    <> ", {throwOnError: false});"
    where
        b = fromIntegral landed / (fromIntegral pf :: Double)
        (a, eqnA) = stopWorkingSubA dw reachStats td
        (v, eqnV) = stopWorkingCase bt a b

spacer :: DomBuilder t m => m ()
spacer = elClass "div" "spacer" $ return ()

viewValidity
    :: MonadWidget t m
    => Dynamic t UtcOffset
    -> Dynamic t Task
    -> Dynamic t (Maybe Vy.Validity)
    -> Dynamic t (Maybe Vy.Validity)
    -> Dynamic t (Maybe ValidityWorking)
    -> Dynamic t (Maybe ReachStats)
    -> Dynamic t (Maybe [(Pilot, TrackReach)])
    -> Dynamic t (Maybe [(Pilot, TrackReach)])
    -> Dynamic t (Maybe TaskDistance)
    -> Dynamic t (Maybe [(Pilot, FlyingSection UTCTime)])
    -> m ()
viewValidity utcOffset task vy vyNorm vw reachStats reach bonusReach td flyingTimes = do
    let (landedByStop, stillFlying) =
            splitDynPure
            $ ffor2 task (fromMaybe [] <$> flyingTimes) (\Task{stopped} ft ->
                maybe
                    (ft, [])
                    (\TaskStop{retroactive = t} ->
                        partition (maybe False ((< t) . snd) . snd) ft)
                    stopped)

    _ <- dyn $ ffor3 vy vyNorm vw (\vy' vyNorm' vw' ->
        dyn $ ffor3 reachStats td landedByStop (\reachStats' td' lo ->
            case (vy', vyNorm', vw', reachStats', td') of
                (Nothing, _, _, _, _) -> text "Loading validity ..."
                (_, Nothing, _, _, _) -> text "Loading expected validity from FS ..."
                (_, _, Nothing, _, _) -> text "Loading validity workings ..."
                (_, _, _, Nothing, _) -> text "Loading reach stats ..."
                (_, _, _, _, Nothing) -> text "Loading stopped task distance ..."
                (Just v, Just vN, Just w, Just r, Just d) -> do
                    elAttr
                        "a"
                        (("class" =: "button") <> ("onclick" =: hookWorking v w r d (length lo)))
                        (text "Show Working")

                    spacer
                    viewDay v vN w
                    spacer
                    viewLaunch v w
                    spacer
                    viewDistance v w
                    spacer
                    viewTime v w
                    spacer

                    viewStop
                        utcOffset
                        v
                        w
                        reachStats
                        (fromMaybe [] <$> reach)
                        (fromMaybe [] <$> bonusReach)
                        d
                        landedByStop
                        stillFlying

                    spacer
                    return ()))

    return ()

viewDay
    :: DomBuilder t m
    => Vy.Validity
    -> Vy.Validity
    -> ValidityWorking
    -> m ()
viewDay
    Vy.Validity{task = dq, launch = lv, distance = dv, time = tv, stop = sv}
    Vy.Validity{task = dqN, launch = lvN, distance = dvN, time = tvN, stop = svN}
    ValidityWorking{launch = LaunchValidityWorking{..}} = do
    elClass "div" "card" $ do
        elClass "div" "card-content" $ do
            elClass "p" "level subtitle is-6" $ do
                elClass "span" "level-item level-left" $
                    elClass "h2" "title is-4" . text
                        $ "Task Validity* = " <> Vy.showTaskValidity dq

                elClass "span" "level-item level-right" $
                    text "* Day Quality"

            elClass "table" "table is-striped" $ do
                el "thead" $ do
                    el "tr" $ do
                        el "th" $ text ""
                        el "th" $ text ""
                        elClass "th" "validity" $ text "Validity"
                        elClass "th" "th-norm validity" $ text "✓"
                        elClass "th" "th-norm th-diff" $ text "Δ"

                let elV = elClass "td" "validity" . text
                let elN = elClass "td" "td-norm" . text
                let elD = elClass "td" "td-norm td-diff" . text

                let elV' = elClass "th" "validity" . text
                let elN' = elClass "th" "td-norm" . text
                let elD' = elClass "th" "td-norm td-diff" . text

                el "tbody" $ do
                    el "tr" $ do
                        el "td" $ text "lv"
                        el "td" $ text "Launch"
                        elV $ Vy.showLaunchValidity lv
                        elN $ Vy.showLaunchValidity lvN
                        elD $ Vy.showLaunchValidityDiff lvN lv
                        return ()

                    el "tr" $ do
                        el "td" $ text "dv"
                        el "td" $ text "Distance"
                        elV $ Vy.showDistanceValidity dv
                        elN $ Vy.showDistanceValidity dvN
                        elD $ Vy.showDistanceValidityDiff dvN dv
                        return ()

                    el "tr" $ do
                        el "td" $ text "tv"
                        el "td" $ text "Time"
                        elV $ Vy.showTimeValidity tv
                        elN $ Vy.showTimeValidity tvN
                        elD $ Vy.showTimeValidityDiff tvN tv
                        return ()

                    el "tr" $ do
                        el "td" $ text "sv"
                        el "td" $ text "Stop"
                        elV $ Vy.showStopValidity sv
                        elN $ Vy.showStopValidity svN
                        elD $ Vy.showStopValidityDiff svN sv
                        return ()

                    el "tr" $ do
                        el "th" $ text ""
                        el "th" $ text "Task"
                        elV' $ Vy.showTaskValidity dq
                        elN' $ Vy.showTaskValidity dqN
                        elD' $ Vy.showTaskValidityDiff dqN dq
                        return ()

                let tdFoot = elAttr "td" ("colspan" =: "4")
                let foot = el "tr" . tdFoot . text

                el "tfoot" $ do
                    foot "✓ An expected value as calculated by the official scoring program, FS."
                    foot "Δ A difference between a value and an expected value."
                    return ()
                return ()

            elAttr
                "div"
                ("id" =: "task-working")
                (text "")

    return ()

viewLaunch
    :: DomBuilder t m
    => Vy.Validity
    -> ValidityWorking
    -> m ()
viewLaunch
    Vy.Validity{launch = v}
    ValidityWorking{launch = LaunchValidityWorking{..}} = do
    elClass "div" "card" $ do
        elClass "div" "card-content" $ do
            elClass "h2" "title is-4" . text
                $ "Launch Validity = " <> Vy.showLaunchValidity v
            elClass "div" "field is-grouped is-grouped-multiline" $ do
                elClass "div" "control" $ do
                    elClass "div" "tags has-addons" $ do
                        elClass "span" "tag" $ do text "f = pilots flying"
                        elClass "span" "tag is-info"
                            $ text (T.pack . show $ flying)
                elClass "div" "control" $ do
                    elClass "div" "tags has-addons" $ do
                        elClass "span" "tag" $ do text "p = pilots present"
                        elClass "span" "tag is-success"
                            $ text (T.pack . show $ present)
                elClass "div" "control" $ do
                    elClass "div" "tags has-addons" $ do
                        elClass "span" "tag" $ do text "n = nominal launch"
                        elClass "span" "tag is-primary"
                            $ text (T.pack . show $ nominalLaunch)

            elAttr
                "div"
                ("id" =: "launch-working")
                (text "")

    return ()

viewDistance
    :: DomBuilder t m
    => Vy.Validity
    -> ValidityWorking
    -> m ()
viewDistance
    Vy.Validity{distance = v}
    ValidityWorking{distance = DistanceValidityWorking{..}} = do
    elClass "div" "card" $ do
        elClass "div" "card-content" $ do
            elClass "h2" "title is-4" . text
                $ "Distance Validity = " <> Vy.showDistanceValidity v
            elClass "div" "field is-grouped is-grouped-multiline" $ do
                elClass "div" "control" $ do
                    elClass "div" "tags has-addons" $ do
                        elClass "span" "tag" $ do text "f = pilots flying"
                        elClass "span" "tag is-info"
                            $ text (T.pack . show $ flying)
                elClass "div" "control" $ do
                    elClass "div" "tags has-addons" $ do
                        elClass "span" "tag" $ do text "area"
                        elClass "span" "tag is-success"
                            $ text (T.pack . show $ area)
                elClass "div" "control" $ do
                    elClass "div" "tags has-addons" $ do
                        elClass "span" "tag" $ do text "ng = nominal goal"
                        elClass "span" "tag is-primary"
                            $ text (T.pack . show $ nominalGoal)
            elClass "div" "field is-grouped is-grouped-multiline" $ do
                elClass "div" "control" $ do
                    elClass "div" "tags has-addons" $ do
                        elClass "span" "tag" $ do text "sum = sum of distance"
                        elClass "span" "tag is-dark"
                            $ text (T.pack . show $ sum)
                elClass "div" "control" $ do
                    elClass "div" "tags has-addons" $ do
                        elClass "span" "tag" $ do text "nd = nominal distance"
                        elClass "span" "tag is-dark"
                            $ text (T.pack . show $ nominalDistance)
                elClass "div" "control" $ do
                    elClass "div" "tags has-addons" $ do
                        elClass "span" "tag" $ do text "md = minimum distance"
                        elClass "span" "tag is-dark"
                            $ text (T.pack . show $ minimumDistance)
                elClass "div" "control" $ do
                    elClass "div" "tags has-addons" $ do
                        elClass "span" "tag" $ do text "bd = best distance"
                        elClass "span" "tag is-dark"
                            $ text (T.pack . show $ bestDistance)

            elAttr
                "div"
                ("id" =: "distance-working")
                (text "")

    return ()

viewTime
    :: DomBuilder t m
    => Vy.Validity
    -> ValidityWorking
    -> m ()
viewTime
    Vy.Validity{time = v}
    ValidityWorking{time = TimeValidityWorking{..}} = do
    elClass "div" "card" $ do
        elClass "div" "card-content" $ do
            elClass "h2" "title is-4" . text
                $ "Time Validity = " <> Vy.showTimeValidity v
            elClass "div" "field is-grouped is-grouped-multiline" $ do
                elClass "div" "control" $ do
                    elClass "div" "tags has-addons" $ do
                        elClass "span" "tag" $ do text "ss best time"
                        elClass "span" "tag is-info"
                            $ text (T.pack . show $ ssBestTime)
                elClass "div" "control" $ do
                    elClass "div" "tags has-addons" $ do
                        elClass "span" "tag" $ do text "bt = gs best time"
                        elClass "span" "tag is-success"
                            $ text (T.pack . show $ gsBestTime)
                elClass "div" "control" $ do
                    elClass "div" "tags has-addons" $ do
                        elClass "span" "tag" $ do text "nt = nominal time"
                        elClass "span" "tag is-primary"
                            $ text (T.pack . show $ nominalTime)
            elClass "div" "field is-grouped is-grouped-multiline" $ do
                elClass "div" "control" $ do
                    elClass "div" "tags has-addons" $ do
                        elClass "span" "tag" $ do text "bd = best distance"
                        elClass "span" "tag is-dark"
                            $ text (T.pack . show $ bestDistance)
                elClass "div" "control" $ do
                    elClass "div" "tags has-addons" $ do
                        elClass "span" "tag" $ do text "nd = nominal distance"
                        elClass "span" "tag is-dark"
                            $ text (T.pack . show $ nominalDistance)

            elAttr
                "div"
                ("id" =: "time-working")
                (text "")

            elClass "div" "notification" $ do
                el "p" $ text "ss best time = the best time ignoring start gates"
                el "p" $ text "gs best time = the best time from the start gate taken"

    return ()

viewStop
    :: MonadWidget t m
    => Dynamic t UtcOffset
    -> Vy.Validity
    -> ValidityWorking
    -> Dynamic t (Maybe ReachStats)
    -> Dynamic t [(Pilot, TrackReach)]
    -> Dynamic t [(Pilot, TrackReach)]
    -> TaskDistance
    -> Dynamic t [(Pilot, FlyingSection UTCTime)]
    -> Dynamic t [(Pilot, FlyingSection UTCTime)]
    -> m ()
viewStop _ Vy.Validity{stop = Nothing} _ _ _ _ _ _ _ = return ()
viewStop
    utcOffset
    Vy.Validity{stop = v}
    ValidityWorking
        { distance = DistanceValidityWorking{..}
        }
    reachStats
    reach
    bonusReach
    td
    landedByStop
    stillFlying = do

    elClass "div" "card" $ do
        elClass "div" "card-content" $ do
            elClass "h2" "title is-4" . text
                $ "Stop Validity = " <> Vy.showStopValidity v
            elClass "div" "field is-grouped is-grouped-multiline" $ do
                elClass "div" "control" $ do
                    elClass "div" "tags has-addons" $ do
                        elClass "span" "tag" $ do text "f = pilots flying"
                        elClass "span" "tag is-info"
                            $ text (T.pack . show $ flying)
                _ <- dyn $ ffor landedByStop (\xs -> do
                    elClass "div" "control" $ do
                        elClass "div" "tags has-addons" $ do
                            elClass "span" "tag" $ do text "ls = pilots landed before stop time"
                            elClass "span" "tag is-warning"
                                $ text (T.pack . show $ length xs))
                elClass "div" "control" $ do
                    elClass "div" "tags has-addons" $ do
                        elClass "span" "tag" $ do text "bd = best distance"
                        elClass "span" "tag is-dark"
                            $ text (T.pack . show $ bestDistance)
                elClass "div" "control" $ do
                    elClass "div" "tags has-addons" $ do
                        elClass "span" "tag" $ do text "ed = launch to ESS distance"
                        elClass "span" "tag is-success" . text
                            $ showTaskDistance td
                _ <- dyn $ ffor reachStats (\case
                    Nothing -> return ()
                    Just ReachStats{..} -> do
                        elClass "div" "field is-grouped is-grouped-multiline" $ do
                            elClass "div" "control" $ do
                                elClass "div" "tags has-addons" $ do
                                    elClass "span" "tag" $ do text "mean flown"
                                    elClass "span" "tag is-primary" . text
                                        $ showPilotDistance 3 flownMean <> " km"
                            elClass "div" "control" $ do
                                elClass "div" "tags has-addons" $ do
                                    elClass "span" "tag" $ do text "flown standard deviation"
                                    elClass "span" "tag is-primary" . text
                                        $ showPilotDistance 3 flownStdDev <> " km"
                            elClass "div" "control" $ do
                                elClass "div" "tags has-addons" $ do
                                    elClass "span" "tag" $ do text "mean reach"
                                    elClass "span" "tag is-danger" . text
                                        $ showPilotDistance 3 reachMean <> " km"
                            elClass "div" "control" $ do
                                elClass "div" "tags has-addons" $ do
                                    elClass "span" "tag" $ do text "reach standard deviation"
                                    elClass "span" "tag is-danger" . text
                                        $ showPilotDistance 3 reachStdDev <> " km")

                return ()

            spacer

            elAttr
                "div"
                ("id" =: "stop-working")
                (text "")

            spacer

            elClass "div" "tile is-ancestor" $ do
                elClass "div" "tile is-vertical is-6" $
                    elClass "div" "tile" $
                        elClass "div" "tile is-parent is-vertical" $ do
                            elClass "article" "tile is-child box" $ do
                                elClass "p" "title" $ text "Landed"
                                elClass "p" "subtitle" $ text "landed before stop"
                                elClass "div" "content"
                                    $ tablePilotFlyingTimes utcOffset landedByStop

                            elClass "article" "tile is-child box" $ do
                                elClass "p" "title" $ text "Flying"
                                elClass "p" "subtitle" $ text "still flying at stop"
                                elClass "div" "content"
                                    $ tablePilotFlyingTimes utcOffset stillFlying

                elClass "div" "tile is-vertical is-6" $
                    elClass "div" "tile" $
                        elClass "div" "tile is-parent is-vertical" $ do
                            elClass "article" "tile is-child box" $ do
                                elClass "p" "title" $ text "Reach"
                                elClass "p" "subtitle" $ text "reach at stop"
                                elClass "div" "content"
                                    $ tablePilotReach reach bonusReach
    return ()

tablePilotFlyingTimes
    :: MonadWidget t m
    => Dynamic t UtcOffset
    -> Dynamic t [(Pilot, FlyingSection UTCTime)]
    -> m ()
tablePilotFlyingTimes utcOffset xs = do
    tz <- sample . current $ timeZone <$> utcOffset
    _ <- elClass "table" "table is-striped" $ do
            el "thead" $ do
                el "tr" $ do
                    el "th" $ text "Landed"
                    el "th" $ text "Pilot"

                    return ()

            el "tbody" $ do
                simpleList xs (uncurry (rowFlyingTimes tz) . splitDynPure)

    return ()

rowFlyingTimes
    :: MonadWidget t m
    => TimeZone
    -> Dynamic t Pilot
    -> Dynamic t (FlyingSection UTCTime)
    -> m ()
rowFlyingTimes tz p tm = do
    let t = maybe "-" (T.pack . showTime tz . snd) <$> tm
    el "tr" $ do
        el "td" $ dynText t
        el "td" . dynText $ showPilotName <$> p

        return ()

tablePilotReach
    :: MonadWidget t m
    => Dynamic t [(Pilot, TrackReach)]
    -> Dynamic t [(Pilot, TrackReach)]
    -> m ()
tablePilotReach reach bonusReach = do
    let tdFoot = elAttr "td" ("colspan" =: "4")
    let foot = el "tr" . tdFoot . text

    _ <- elClass "table" "table is-striped" $ do
            el "thead" $ do
                el "tr" $ do
                    elAttr "th" ("colspan" =: "3") $ text "Reach (km)"
                    el "th" $ text ""

                    return ()

            el "thead" $ do
                el "tr" $ do
                    elClass "th" "th-plot-reach" $ text "Flown"
                    elClass "th" "th-plot-reach-bonus" $ text "Scored †"
                    elClass "th" "th-plot-reach-bonus-diff" $ text "Δ"
                    el "th" $ text "Pilot"

                    return ()

            _ <- dyn $ ffor bonusReach (\br -> do
                    let mapR = Map.fromList br

                    el "tbody" $
                        simpleList reach (uncurry (rowReachBonus mapR) . splitDynPure))

            el "tfoot" $ do
                foot "† Reach as scored."
                foot "Δ Altitude bonus reach."
            return ()
    return ()

rowReachBonus
    :: MonadWidget t m
    => Map Pilot TrackReach
    -> Dynamic t Pilot
    -> Dynamic t TrackReach
    -> m ()
rowReachBonus mapR p r = do
    (bReach, diffReach) <- sample . current
            $ ffor2 p r (\p' r' ->
                case Map.lookup p' mapR of
                    Just br ->
                        let rBonus = reach $ br
                            rFlown = reach $ r'
                        in
                            ( showPilotDistance 3 $ reach br
                            , showPilotDistanceDiff rFlown rBonus
                            )

                    _ -> ("", ""))

    el "tr" $ do
        elClass "td" "td-plot-reach" . dynText $ showPilotDistance 3 . reach <$> r
        elClass "td" "td-plot-reach-bonus" $ text bReach
        elClass "td" "td-plot-reach-bonus-diff" $ text diffReach

        el "td" . dynText $ showPilotName <$> p

        return ()

showPilotDistanceDiff :: PilotDistance -> PilotDistance -> T.Text
showPilotDistanceDiff (PilotDistance expected) (PilotDistance actual)
    | printf "%.3f" actual == (printf "%.3f" expected :: String) = "="
    | otherwise = T.pack . printf "%+.3f" $ actual - expected
