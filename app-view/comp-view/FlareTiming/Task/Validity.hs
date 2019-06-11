module FlareTiming.Task.Validity (viewValidity) where

import Prelude hiding (sum)
import qualified Prelude as Stats (sum)
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
    , StopValidityWorking(..)
    , BestTime(..)
    , PilotsFlying(..)
    , NominalGoal(..)
    , MinimumDistance(..)
    , MaximumDistance(..)
    , NominalDistance(..)
    , NominalDistanceArea(..)
    , showPilotsPresentDiff
    , showPilotsFlyingDiff
    , showPilotsLandedDiff
    , showPilotsAtEssDiff
    , showNominalLaunchDiff
    , showNominalGoal, showNominalGoalDiff
    , showBestTime, showBestTimeDiff
    , showNominalTime, showNominalTimeDiff
    , showBestDistance, showBestDistanceDiff
    , showLaunchToEss, showLaunchToEssDiff
    , showSumOfDistance, showSumOfDistanceDiff
    , showNominalDistance, showNominalDistanceDiff
    , showNominalDistanceArea, showNominalDistanceAreaDiff
    , showMinimumDistance, showMinimumDistanceDiff
    , showMaximumDistance, showMaximumDistanceDiff
    )
import WireTypes.Cross (FlyingSection)
import WireTypes.Route (TaskDistance(..), showTaskDistance)
import WireTypes.Reach (TrackReach(..), ReachStats(..))
import WireTypes.Point (PilotDistance(..), showPilotDistance)
import WireTypes.Pilot (Pilot(..))
import WireTypes.Comp (Task(..), UtcOffset(..), TaskStop(..))
import FlareTiming.Pilot (showPilotName)
import FlareTiming.Time (timeZone, showTime)
import qualified FlareTiming.Statistics as Stats (mean, stdDev)

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
    <> " a &= \\\\sqrt{\\\\frac{bd - \\\\mu(flown)}{ed - bd + 1} * \\\\sqrt{\\\\frac{\\\\sigma(flown)}{5}}}"
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
    -> Dynamic t (Maybe ValidityWorking)
    -> Dynamic t (Maybe ReachStats)
    -> Dynamic t (Maybe [(Pilot, TrackReach)])
    -> Dynamic t (Maybe [(Pilot, TrackReach)])
    -> Dynamic t (Maybe TaskDistance)
    -> Dynamic t (Maybe [(Pilot, FlyingSection UTCTime)])
    -> m ()
viewValidity utcOffset task vy vyNorm vw vwNorm reachStats reach bonusReach td flyingTimes = do
    let (landedByStop, stillFlying) =
            splitDynPure
            $ ffor2 task (fromMaybe [] <$> flyingTimes) (\Task{stopped} ft ->
                maybe
                    (ft, [])
                    (\TaskStop{retroactive = t} ->
                        partition (maybe False ((< t) . snd) . snd) ft)
                    stopped)

    _ <- dyn $ ffor2 vy vyNorm (\vy' vyNorm' ->
        dyn $ ffor2 vw vwNorm (\vw' vwNorm' ->
            dyn $ ffor3 reachStats td landedByStop (\reachStats' td' lo ->
                case (vy', vyNorm', vw', vwNorm', reachStats', td') of
                    (Nothing, _, _, _, _, _) -> text "Loading validity ..."
                    (_, Nothing, _, _, _, _) -> text "Loading expected validity from FS ..."
                    (_, _, Nothing, _, _, _) -> text "Loading validity workings ..."
                    (_, _, _, Nothing, _, _) -> text "Loading expected validity workings from FS ..."
                    (_, _, _, _, Nothing, _) -> text "Loading reach stats ..."
                    (_, _, _, _, _, Nothing) -> text "Loading stopped task distance ..."
                    (Just v, Just vN, Just w, Just wN, Just r, Just d) -> do
                        elAttr
                            "a"
                            (("class" =: "button") <> ("onclick" =: hookWorking v w r d (length lo)))
                            (text "Show Working")

                        spacer
                        viewDay v vN w
                        spacer
                        viewLaunch v vN w wN
                        spacer
                        viewDistance v vN w wN
                        spacer
                        viewTime v vN w wN
                        spacer

                        viewStop
                            utcOffset
                            v vN
                            w wN
                            reachStats
                            (fromMaybe [] <$> reach)
                            (fromMaybe [] <$> bonusReach)
                            d
                            landedByStop
                            stillFlying

                        spacer
                        return ())))

    return ()

elV :: DomBuilder t m => T.Text -> m ()
elV = elClass "td" "validity" . text

elN :: DomBuilder t m => T.Text -> m ()
elN = elClass "td" "td-norm" . text

elD :: DomBuilder t m => T.Text -> m ()
elD = elClass "td" "td-norm td-diff" . text

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
                        elV $ Vy.showTaskValidity dq
                        elN $ Vy.showTaskValidity dqN
                        elD $ Vy.showTaskValidityDiff dqN dq
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
    -> Vy.Validity
    -> ValidityWorking
    -> ValidityWorking
    -> m ()
viewLaunch
    Vy.Validity{launch = lv}
    Vy.Validity{launch = lvN}
    ValidityWorking
        { launch =
            LaunchValidityWorking
                { flying
                , present
                , nominalLaunch
                }
        }
    ValidityWorking
        { launch =
            LaunchValidityWorking
                { flying = flyingN
                , present = presentN
                , nominalLaunch = nominalLaunchN
                }
        }
    = do
    elClass "div" "card" $ do
        elClass "div" "card-content" $ do
            elClass "h2" "title is-4" . text
                $ "Launch Validity = " <> Vy.showLaunchValidity lv

            elClass "table" "table is-striped" $ do
                el "thead" $ do
                    el "tr" $ do
                        elAttr "th" ("colspan" =: "3") $ text ""
                        elClass "th" "th-norm validity" $ text "✓"
                        elClass "th" "th-norm th-diff" $ text "Δ"

                el "tbody" $ do
                    el "tr" $ do
                        el "td" $ text "f"
                        el "td" $ text "Pilots Flying"
                        elV . T.pack $ show flying
                        elN . T.pack $ show flyingN
                        elD $ showPilotsFlyingDiff flyingN flying
                        return ()

                    el "tr" $ do
                        el "td" $ text "p"
                        el "td" $ text "Pilots Present"
                        elV . T.pack $ show present
                        elN . T.pack $ show presentN
                        elD $ showPilotsPresentDiff presentN present
                        return ()

                    el "tr" $ do
                        el "td" $ text "n"
                        el "td" $ text "Nominal Launch"
                        elV . T.pack $ show nominalLaunch
                        elN . T.pack $ show nominalLaunchN
                        elD $ showNominalLaunchDiff nominalLaunchN nominalLaunch
                        return ()

                    el "tr" $ do
                        el "th" $ text ""
                        el "th" $ text "Launch Validity"
                        elV $ Vy.showLaunchValidity lv
                        elN $ Vy.showLaunchValidity lvN
                        elD $ Vy.showLaunchValidityDiff lvN lv
                        return ()

                return ()

            elAttr
                "div"
                ("id" =: "launch-working")
                (text "")

    return ()

viewDistance
    :: DomBuilder t m
    => Vy.Validity
    -> Vy.Validity
    -> ValidityWorking
    -> ValidityWorking
    -> m ()
viewDistance
    Vy.Validity{distance = dv}
    Vy.Validity{distance = dvN}
    ValidityWorking
        { distance =
            DistanceValidityWorking
                { sum = sd
                , flying = flying
                , area
                , nominalGoal = ng
                , nominalDistance = nd
                , minimumDistance = md
                , bestDistance = bd
                }
        }
    ValidityWorking
        { distance =
            DistanceValidityWorking
                { sum = sdN
                , flying = flyingN
                , nominalGoal = ngN
                , nominalDistance = ndN
                , minimumDistance = mdN
                , bestDistance = bdN
                }
        }
    = do
    elClass "div" "card" $ do
        elClass "div" "card-content" $ do
            elClass "h2" "title is-4" . text
                $ "Distance Validity = " <> Vy.showDistanceValidity dv

            elClass "table" "table is-striped" $ do
                el "thead" $ do
                    el "tr" $ do
                        elAttr "th" ("colspan" =: "3") $ text ""
                        elClass "th" "th-norm validity" $ text "✓"
                        elClass "th" "th-norm th-diff" $ text "Δ"

                el "tbody" $ do
                    el "tr" $ do
                        el "td" $ text "f"
                        el "td" $ text "Pilots Flying"
                        elV . T.pack $ show flying
                        elN . T.pack $ show flyingN
                        elD $ showPilotsFlyingDiff flyingN flying
                        return ()

                    el "tr" $ do
                        el "td" $ text "ng"
                        el "td" $ text "Nominal Goal"
                        elV $ showNominalGoal ng
                        elN $ showNominalGoal ngN
                        elD $ showNominalGoalDiff ngN ng
                        return ()

                    el "tr" $ do
                        el "td" $ text "sd"
                        el "td" $ text "Sum of Distance †"
                        elV $ showSumOfDistance sd
                        elN $ showSumOfDistance sdN
                        elD $ showSumOfDistanceDiff sdN sd
                        return ()

                    el "tr" $ do
                        el "td" $ text "nd"
                        el "td" $ text "Nominal Distance"
                        elV $ showNominalDistance nd
                        elN $ showNominalDistance ndN
                        elD $ showNominalDistanceDiff ndN nd
                        return ()

                    el "tr" $ do
                        el "td" $ text "md"
                        el "td" $ text "Minimum Distance"
                        elV $ showMinimumDistance md
                        elN $ showMinimumDistance mdN
                        elD $ showMinimumDistanceDiff mdN md
                        return ()

                    el "tr" $ do
                        el "td" $ text "bd"
                        el "td" $ text "Best Distance"
                        elV $ showMaximumDistance bd
                        elN $ showMaximumDistance bdN
                        elD $ showMaximumDistanceDiff bdN bd
                        return ()

                    let aN =
                            let NominalGoal ng' = ngN
                                NominalDistance nd' = ndN
                                MinimumDistance md' = mdN
                            in (ng' + 1) * (nd' - md')

                    let bN =
                            let NominalGoal ng' = ngN
                                NominalDistance nd' = ndN
                                MaximumDistance bd' = bdN
                            in max 0 $ ng'* (bd' - nd')

                    let areaN = NominalDistanceArea $ (aN + bN) / 2

                    el "tr" $ do
                        el "td" $ text "area"
                        el "th" $ text "Nominal Distance Area"
                        elV $ showNominalDistanceArea area
                        elN $ showNominalDistanceArea areaN
                        elD $ showNominalDistanceAreaDiff areaN area
                        return ()

                    el "tr" $ do
                        el "th" $ text ""
                        el "th" $ text "Distance Validity"
                        elV $ Vy.showDistanceValidity dv
                        elN $ Vy.showDistanceValidity dvN
                        elD $ Vy.showDistanceValidityDiff dvN dv
                        return ()

                let tdFoot = elAttr "td" ("colspan" =: "5")
                let foot = el "tr" . tdFoot . text

                el "tfoot" $ foot "† The sum of flown distance further than minimum distance."

            elAttr
                "div"
                ("id" =: "distance-working")
                (text "")

    return ()

viewTime
    :: DomBuilder t m
    => Vy.Validity
    -> Vy.Validity
    -> ValidityWorking
    -> ValidityWorking
    -> m ()
viewTime
    Vy.Validity{time = tv}
    Vy.Validity{time = tvN}
    ValidityWorking
        { time =
            TimeValidityWorking
                { ssBestTime
                , gsBestTime = bt
                , nominalTime = nt
                , bestDistance = bd
                , nominalDistance = nd
                }
        }
    ValidityWorking
        { time =
            TimeValidityWorking
                { gsBestTime = btN
                , nominalTime = ntN
                , bestDistance = bdN
                , nominalDistance = ndN
                }
        }
    = do
    elClass "div" "card" $ do
        elClass "div" "card-content" $ do
            elClass "h2" "title is-4" . text
                $ "Time Validity = " <> Vy.showTimeValidity tv

            elClass "table" "table is-striped" $ do
                el "thead" $ do
                    el "tr" $ do
                        elAttr "th" ("colspan" =: "3") $ text ""
                        elClass "th" "th-norm validity" $ text "✓"
                        elClass "th" "th-norm th-diff" $ text "Δ"

                el "tbody" $ do
                    el "tr" $ do
                        el "td" $ text ""
                        el "td" $ text "Section Best Time †"
                        elV $ showBestTime ssBestTime
                        elN $ ""
                        elD $ ""
                        return ()

                    el "tr" $ do
                        el "td" $ text "bt"
                        el "td" $ text "Gate Best Time ‡"
                        elV $ showBestTime bt
                        elN $ showBestTime btN
                        elD $ showBestTimeDiff btN bt
                        return ()

                    el "tr" $ do
                        el "td" $ text "nt"
                        el "td" $ text "Nominal Time"
                        elV $ showNominalTime nt
                        elN $ showNominalTime ntN
                        elD $ showNominalTimeDiff ntN nt
                        return ()

                    el "tr" $ do
                        el "td" $ text "bd"
                        el "td" $ text "Best Distance"
                        elV $ showBestDistance bd
                        elN $ showBestDistance bdN
                        elD $ showBestDistanceDiff bdN bd
                        return ()

                    el "tr" $ do
                        el "td" $ text "nd"
                        el "td" $ text "Nominal Distance"
                        elV $ showNominalDistance nd
                        elN $ showNominalDistance ndN
                        elD $ showNominalDistanceDiff ndN nd
                        return ()

                    el "tr" $ do
                        el "th" $ text ""
                        el "th" $ text "Time Validity"
                        elV $ Vy.showTimeValidity tv
                        elN $ Vy.showTimeValidity tvN
                        elD $ Vy.showTimeValidityDiff tvN tv
                        return ()

                let tdFoot = elAttr "td" ("colspan" =: "5")
                let foot = el "tr" . tdFoot . text

                el "tfoot" $ do
                    foot "† The best time ignoring start gates."
                    foot "‡ The best time from the start gate taken."
                    return ()

            elAttr
                "div"
                ("id" =: "time-working")
                (text "")

    return ()

viewStop
    :: MonadWidget t m
    => Dynamic t UtcOffset
    -> Vy.Validity
    -> Vy.Validity
    -> ValidityWorking
    -> ValidityWorking
    -> Dynamic t (Maybe ReachStats)
    -> Dynamic t [(Pilot, TrackReach)]
    -> Dynamic t [(Pilot, TrackReach)]
    -> TaskDistance
    -> Dynamic t [(Pilot, FlyingSection UTCTime)]
    -> Dynamic t [(Pilot, FlyingSection UTCTime)]
    -> m ()
viewStop _ Vy.Validity{stop = Nothing} _ _ _ _ _ _ _ _ _ = return ()
viewStop _ _ Vy.Validity{stop = Nothing} _ _ _ _ _ _ _ _ = return ()
viewStop _ _ _ ValidityWorking{stop = Nothing} _ _ _ _ _ _ _ = return ()
viewStop _ _ _ _ ValidityWorking{stop = Nothing} _ _ _ _ _ _ = return ()
viewStop
    utcOffset
    Vy.Validity{stop = sv}
    Vy.Validity{stop = svN}
    ValidityWorking
        { stop =
            Just StopValidityWorking
                { pilotsAtEss
                , flying
                , landed
                , flownMax = bd
                , flownMean
                , flownStdDev
                , launchToEssDistance = ed
                }
        }
    ValidityWorking
        { stop =
            Just StopValidityWorking
                { pilotsAtEss = pilotsAtEssN
                , flying = flyingN
                , landed = landedN
                , flownMax = bdN
                , flownMean = flownMeanN
                , flownStdDev = flownStdDevN
                , launchToEssDistance = edN
                }
        }
    reachStats
    reach
    bonusReach
    _td
    landedByStop
    stillFlying = do

    elClass "div" "card" $ do
        elClass "div" "card-content" $ do
            elClass "h2" "title is-4" . text
                $ "Stop Validity = " <> Vy.showStopValidity sv

            elClass "table" "table is-striped" $ do
                el "thead" $ do
                    el "tr" $ do
                        elAttr "th" ("colspan" =: "3") $ text ""
                        elClass "th" "th-norm validity" $ text "✓"
                        elClass "th" "th-norm th-diff" $ text "Δ"

                el "tbody" $ do
                    el "tr" $ do
                        el "td" $ text ""
                        el "td" $ text "Pilots at ESS"
                        elV . T.pack $ show pilotsAtEss
                        elN . T.pack $ show pilotsAtEssN
                        elD $ showPilotsAtEssDiff pilotsAtEssN pilotsAtEss
                        return ()

                    el "tr" $ do
                        el "td" $ text "f"
                        el "td" $ text "Pilots Flying"
                        elV . T.pack $ show flying
                        elN . T.pack $ show flyingN
                        elD $ showPilotsFlyingDiff flyingN flying
                        return ()

                    el "tr" $ do
                        el "td" $ text "ls"
                        el "td" $ text "Pilots Landed before Stop"
                        elV . T.pack $ show landed
                        elN . T.pack $ show landedN
                        elD $ showPilotsLandedDiff landedN landed
                        return ()

                    el "tr" $ do
                        el "td" $ text "bd"
                        el "td" $ text "Best Distance"
                        elV $ showBestDistance bd
                        elN $ showBestDistance bdN
                        elD $ showBestDistanceDiff bdN bd
                        return ()

                    el "tr" $ do
                        el "td" $ text "ed"
                        el "td" $ text "Launch to ESS Distance"
                        elV $ showLaunchToEss ed
                        elN $ showLaunchToEss edN
                        elD $ showLaunchToEssDiff edN ed
                        return ()

                    _ <- dyn $ ffor reachStats (\case
                        Nothing -> do
                            el "tr" $ do
                                elAttr "th" ("rowspan" =: "3") $ text "μ"
                                el "td" $ text "Reach †"
                                elV $ "n/a"
                                elN $ "n/a"
                                elD $ ""
                                return ()

                            el "tr" $ do
                                el "th" $ text "Flown ‡"
                                elV $ "n/a"
                                elN $ "n/a"
                                elD $ ""
                                return ()

                        Just
                            ReachStats
                                { flownMean = flownMeanR
                                , reachMean = reachMeanR
                                } -> do
                            el "tr" $ do
                                elAttr "th" ("rowspan" =: "3") $ text "μ"
                                el "td" $ text "Reach †"
                                elV $ showPilotDistance 3 reachMeanR <> " km"
                                elN $ "n/a"
                                elD $ ""
                                return ()

                            el "tr" $ do
                                el "th" $ text "Flown ‡"
                                elV $ showPilotDistance 3 flownMeanR <> " km"
                                elN $ "n/a"
                                elD $ ""
                                return ())

                    el "tr" $ do
                        el "td" $ text "Extra ‖"
                        elV $ showPilotDistance 3 flownMean <> " km"
                        elN $ showPilotDistance 3 flownMeanN <> " km"
                        elD $ ""
                        return ()

                    _ <- dyn $ ffor reachStats (\case
                        Nothing -> do
                            el "tr" $ do
                                elAttr "th" ("rowspan" =: "3") $ text "σ"
                                el "td" $ text "Reach †"
                                elV $ "n/a"
                                elN $ "n/a"
                                elD $ ""
                                return ()

                            el "tr" $ do
                                el "th" $ text "Flown ‡"
                                elV $ "n/a"
                                elN $ "n/a"
                                elD $ ""
                                return ()

                        Just
                            ReachStats
                                { flownStdDev = flownStdDevR
                                , reachStdDev = reachStdDevR
                                } -> do
                            el "tr" $ do
                                elAttr "th" ("rowspan" =: "3") $ text "σ"
                                el "td" $ text "Reach †"
                                elV $ showPilotDistance 3 reachStdDevR <> " km"
                                elN $ "n/a"
                                elD $ ""
                                return ()

                            el "tr" $ do
                                el "th" $ text "Flown ‡"
                                elV $ showPilotDistance 3 flownStdDevR <> " km"
                                elN $ "n/a"
                                elD $ ""
                                return ())

                    el "tr" $ do
                        el "td" $ text "Extra ‖"
                        elV $ showPilotDistance 3 flownStdDev <> " km"
                        elN $ showPilotDistance 3 flownStdDevN <> " km"
                        elD $ ""
                        return ()

                    el "tr" $ do
                        el "th" $ text ""
                        el "th" $ text "Stop Validity"
                        elV $ Vy.showStopValidity sv
                        elN $ Vy.showStopValidity svN
                        elD $ Vy.showStopValidityDiff svN sv
                        return ()

                let tdFoot = elAttr "td" ("colspan" =: "5")
                let foot = el "tr" . tdFoot . text

                el "tfoot" $ do
                    foot "μ Mean."
                    foot "σ Standard Deviation."
                    foot "† With reach as small as actually flown."
                    foot "‡ With reach clamped below to be no smaller than minimum distance."
                    foot "‖ With altitude above goal converted to extra reach via glide."
                    return ()

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
    let tdFoot = elAttr "td" ("colspan" =: "5")
    let foot = el "tr" . tdFoot . text

    _ <- elClass "table" "table is-striped" $ do
            el "thead" $ do
                el "tr" $ do
                    elAttr "th" ("colspan" =: "4") $ text "Reach (km)"
                    el "th" $ text ""

                    return ()

            el "thead" $ do
                el "tr" $ do
                    el "th" $ text ""
                    elClass "th" "th-plot-reach" $ text "Flown †"
                    elClass "th" "th-plot-reach-bonus" $ text "Extra ‡"
                    elClass "th" "th-plot-reach-bonus-diff" $ text "Δ"
                    el "th" $ text "Pilot"

                    return ()

            _ <- el "tbody" . dyn $ ffor2 reach bonusReach (\r br -> do
                    let rs = [d | (_, TrackReach{reach = PilotDistance d}) <- r]
                    let bs = [d | (_, TrackReach{reach = PilotDistance d}) <- br]
                    let ds = zipWith (-) bs rs
                    let mapR = Map.fromList br

                    _ <- simpleList reach (uncurry (rowReachBonus mapR) . splitDynPure)
                    let f = text . T.pack . printf "%.3f"

                    el "tr" $ do
                        el "th" $ text "∑"
                        elClass "td" "td-plot-reach" . f
                            $ Stats.sum rs
                        elClass "td" "td-plot-reach-bonus" . f
                            $ Stats.sum bs
                        elClass "td" "td-plot-reach-bonus-diff" . f
                            $ Stats.sum ds
                        elAttr "td" ("rowspan" =: "3") $ text ""

                        return ()

                    el "tr" $ do
                        el "th" $ text "μ"
                        elClass "td" "td-plot-reach" . f
                            $ Stats.mean rs
                        elClass "td" "td-plot-reach-bonus" . f
                            $ Stats.mean bs
                        elClass "td" "td-plot-reach-bonus-diff" . f
                            $ Stats.mean ds

                        return ()

                    el "tr" $ do
                        el "th" $ text "σ"
                        elClass "td" "td-plot-reach" . f
                            $ Stats.stdDev rs
                        elClass "td" "td-plot-reach-bonus" . f
                            $ Stats.stdDev bs
                        elClass "td" "td-plot-reach-bonus-diff" . f
                            $ Stats.stdDev ds

                        return ()
                    return ())

            el "tfoot" $ do
                foot "† With reach clamped below to be no smaller than minimum distance."
                foot "‡ As scored with altitude above goal converted to extra reach via glide."
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
        el "td" $ text ""
        elClass "td" "td-plot-reach" . dynText $ showPilotDistance 3 . reach <$> r
        elClass "td" "td-plot-reach-bonus" $ text bReach
        elClass "td" "td-plot-reach-bonus-diff" $ text diffReach

        el "td" . dynText $ showPilotName <$> p

        return ()

showPilotDistanceDiff :: PilotDistance -> PilotDistance -> T.Text
showPilotDistanceDiff (PilotDistance expected) (PilotDistance actual)
    | printf "%.3f" actual == (printf "%.3f" expected :: String) = "="
    | otherwise = T.pack . printf "%+.3f" $ actual - expected
