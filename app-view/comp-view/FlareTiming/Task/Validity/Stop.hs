module FlareTiming.Task.Validity.Stop
    ( viewStop
    , stopWorking
    ) where

import Prelude hiding (sum)
import qualified Prelude as Stats (sum)
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (TimeZone)
import Reflex
import Reflex.Dom
import Text.Printf (printf)
import qualified Data.Text as T (Text, pack)
import Data.Map (Map)
import qualified Data.Map.Strict as Map

import qualified WireTypes.Validity as Vy
    ( Validity(..)
    , showStopValidity, showStopValidityDiff
    )
import WireTypes.ValidityWorking
    ( ValidityWorking(..)
    , DistanceValidityWorking(..)
    , TimeValidityWorking(..)
    , StopValidityWorking(..)
    , PilotsFlying(..)
    , MaximumDistance(..)
    , showPilotsFlyingDiff
    , showPilotsLandedDiff
    , showPilotsAtEssDiff
    , showBestDistance, showBestDistanceDiff
    , showLaunchToEss, showLaunchToEssDiff
    )
import WireTypes.Cross (FlyingSection)
import WireTypes.Route (TaskDistance(..), showTaskDistance)
import WireTypes.Reach (TrackReach(..), BolsterStats(..))
import WireTypes.Point (PilotDistance(..), showPilotDistance)
import WireTypes.Pilot (Pilot(..))
import WireTypes.Comp (UtcOffset(..))
import FlareTiming.Pilot (showPilotName)
import FlareTiming.Time (timeZone, showTime)
import qualified FlareTiming.Statistics as Stats (mean, stdDev)
import FlareTiming.Task.Validity.Widget (katexNewLine, spacer, elV, elN, elD)

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

stopWorkingSubA :: DistanceValidityWorking -> BolsterStats -> TaskDistance -> (Double, T.Text)

stopWorkingSubA
    DistanceValidityWorking{bestDistance = bd@(MaximumDistance bd')}
    BolsterStats
        { bolsterMean = mf@(PilotDistance mf')
        , bolsterStdDev = sf@(PilotDistance sf')
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
    -> BolsterStats
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
    <> " bd &= \\\\max(flown)"
    <> katexNewLine
    <> katexNewLine
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

viewStop
    :: MonadWidget t m
    => Dynamic t UtcOffset
    -> Vy.Validity
    -> Vy.Validity
    -> ValidityWorking
    -> ValidityWorking
    -> Dynamic t (Maybe BolsterStats)
    -> Dynamic t (Maybe BolsterStats)
    -> Dynamic t [(Pilot, TrackReach)]
    -> Dynamic t [(Pilot, TrackReach)]
    -> TaskDistance
    -> Dynamic t [(Pilot, FlyingSection UTCTime)]
    -> Dynamic t [(Pilot, FlyingSection UTCTime)]
    -> m ()
viewStop _ Vy.Validity{stop = Nothing} _ _ _ _ _ _ _ _ _ _ = return ()
viewStop _ _ Vy.Validity{stop = Nothing} _ _ _ _ _ _ _ _ _ = return ()
viewStop _ _ _ ValidityWorking{stop = Nothing} _ _ _ _ _ _ _ _ = return ()
viewStop _ _ _ _ ValidityWorking{stop = Nothing} _ _ _ _ _ _ _ = return ()
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
                , extraMax
                , flownMax
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
                , extraMax = extraMaxN
                , flownMax = flownMaxN
                , flownMean = flownMeanN
                , flownStdDev = flownStdDevN
                , launchToEssDistance = edN
                }
        }
    reachStats
    bonusStats
    reach
    bonusReach
    _td
    landedByStop
    stillFlying = do

    elClass "div" "card" $ do
        elClass "div" "card-content" $ do
            elClass "h2" "title is-4" . text
                $ "Stop Validity = " <> Vy.showStopValidity sv

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
                                elClass "p" "subtitle" $ text "reach at or before stop"
                                elClass "div" "content"
                                    $ tablePilotReach reach bonusReach

            spacer

            elClass "table" "table is-striped" $ do
                el "thead" $ do
                    el "tr" $ do
                        elAttr "th" ("colspan" =: "4") $ text ""
                        elClass "th" "th-norm validity" $ text "✓"
                        elClass "th" "th-norm th-diff" $ text "Δ"

                el "tbody" $ do
                    el "tr" $ do
                        elAttr "th" ("rowspan" =: "3") $ text "Pilots"
                        el "td" $ text ""
                        el "td" $ text "at ESS"
                        elV . T.pack $ show pilotsAtEss
                        elN . T.pack $ show pilotsAtEssN
                        elD $ showPilotsAtEssDiff pilotsAtEssN pilotsAtEss
                        return ()

                    el "tr" $ do
                        el "td" $ text "f"
                        el "td" $ text "Flying"
                        elV . T.pack $ show flying
                        elN . T.pack $ show flyingN
                        elD $ showPilotsFlyingDiff flyingN flying
                        return ()

                    el "tr" $ do
                        el "td" $ text "ls"
                        el "td" $ text "Landed before Stop"
                        elV . T.pack $ show landed
                        elN . T.pack $ show landedN
                        elD $ showPilotsLandedDiff landedN landed
                        return ()

                    el "tr" $ do
                        el "th" $ text "Distance"
                        el "td" $ text "ed"
                        el "td" $ text "Launch to ESS"
                        elV $ showLaunchToEss ed
                        elN $ showLaunchToEss edN
                        elD $ showLaunchToEssDiff edN ed
                        return ()

                    el "tr" $ do
                        elAttr "th" ("rowspan" =: "3") $ text "max"
                        el "td" $ text ""
                        el "td" $ text "Reach †"
                        elV $ ""
                        elN $ ""
                        elD $ ""
                        return ()

                    el "tr" $ do
                        el "td" $ text ""
                        el "th" $ text "Bolster ‡"
                        elV $ showBestDistance flownMax
                        elN $ showBestDistance flownMaxN
                        elD $ showBestDistanceDiff flownMaxN flownMax
                        return ()

                    el "tr" $ do
                        el "td" $ text ""
                        el "td" $ text "Extra"
                        elV $ showBestDistance extraMax
                        elN $ showBestDistance extraMaxN
                        elD $ showBestDistanceDiff extraMaxN extraMax
                        return ()

                    _ <- dyn $ ffor reachStats (\case
                        Nothing -> do
                            el "tr" $ do
                                elAttr "th" ("rowspan" =: "5") $ text "μ"
                                el "td" $ text ""
                                el "td" $ text "Reach"
                                elV $ "n/a"
                                elN $ "n/a"
                                elD $ ""
                                return ()

                            el "tr" $ do
                                el "td" $ text ""
                                el "th" $ text "Bolster"
                                elV $ "n/a"
                                elN $ "n/a"
                                elD $ ""
                                return ()

                        Just
                            BolsterStats
                                { bolsterMean = bolsterMeanR
                                , reachMean = reachMeanR
                                } -> do
                            el "tr" $ do
                                elAttr "th" ("rowspan" =: "5") $ text "μ"
                                el "td" $ text ""
                                el "td" $ text "Reach"
                                elV $ showPilotDistance 3 reachMeanR <> " km"
                                elN $ "n/a"
                                elD $ ""
                                return ()

                            el "tr" $ do
                                el "td" $ text ""
                                el "th" $ text "Bolster"
                                elV $ showPilotDistance 3 bolsterMeanR <> " km"
                                elN $ "n/a"
                                elD $ ""
                                return ())

                    _ <- dyn $ ffor bonusStats (\case
                        Nothing -> do
                            el "tr" $ do
                                el "td" $ text ""
                                el "td" $ text "Extra ‖ Reach"
                                elV $ "n/a"
                                elN $ "n/a"
                                elD $ ""
                                return ()

                            el "tr" $ do
                                el "td" $ text ""
                                el "th" $ text "Extra Bolster"
                                elV $ "n/a"
                                elN $ "n/a"
                                elD $ ""
                                return ()

                        Just
                            BolsterStats
                                { bolsterMean = bolsterMeanR
                                , reachMean = reachMeanR
                                } -> do
                            el "tr" $ do
                                el "td" $ text ""
                                el "td" $ text "Extra ‖ Reach"
                                elV $ showPilotDistance 3 reachMeanR <> " km"
                                elN $ "n/a"
                                elD $ ""
                                return ()

                            el "tr" $ do
                                el "td" $ text ""
                                el "th" $ text "Extra Bolster"
                                elV $ showPilotDistance 3 bolsterMeanR <> " km"
                                elN $ "n/a"
                                elD $ ""
                                return ())

                    el "tr" $ do
                        el "td" $ text ""
                        el "td" $ text "Working"
                        elV $ showPilotDistance 3 flownMean <> " km"
                        elN $ showPilotDistance 3 flownMeanN <> " km"
                        elD $ ""
                        return ()

                    _ <- dyn $ ffor reachStats (\case
                        Nothing -> do
                            el "tr" $ do
                                elAttr "th" ("rowspan" =: "5") $ text "σ"
                                el "td" $ text ""
                                el "td" $ text "Reach"
                                elV $ "n/a"
                                elN $ "n/a"
                                elD $ ""
                                return ()

                            el "tr" $ do
                                el "td" $ text ""
                                el "th" $ text "Bolster"
                                elV $ "n/a"
                                elN $ "n/a"
                                elD $ ""
                                return ()

                        Just
                            BolsterStats
                                { bolsterStdDev = bolsterStdDevR
                                , reachStdDev = reachStdDevR
                                } -> do
                            el "tr" $ do
                                elAttr "th" ("rowspan" =: "5") $ text "σ"
                                el "td" $ text ""
                                el "td" $ text "Reach"
                                elV $ showPilotDistance 3 reachStdDevR <> " km"
                                elN $ "n/a"
                                elD $ ""
                                return ()

                            el "tr" $ do
                                el "td" $ text ""
                                el "th" $ text "Bolster"
                                elV $ showPilotDistance 3 bolsterStdDevR <> " km"
                                elN $ "n/a"
                                elD $ ""
                                return ())

                    _ <- dyn $ ffor bonusStats (\case
                        Nothing -> do
                            el "tr" $ do
                                el "td" $ text ""
                                el "td" $ text "Extra Reach"
                                elV $ "n/a"
                                elN $ "n/a"
                                elD $ ""
                                return ()

                            el "tr" $ do
                                el "td" $ text ""
                                el "th" $ text "Extra Bolster"
                                elV $ "n/a"
                                elN $ "n/a"
                                elD $ ""
                                return ()

                        Just
                            BolsterStats
                                { bolsterStdDev = bolsterStdDevR
                                , reachStdDev = reachStdDevR
                                } -> do
                            el "tr" $ do
                                el "td" $ text ""
                                el "td" $ text "Extra Reach"
                                elV $ showPilotDistance 3 reachStdDevR <> " km"
                                elN $ "n/a"
                                elD $ ""
                                return ()

                            el "tr" $ do
                                el "td" $ text ""
                                el "th" $ text "Extra Bolster"
                                elV $ showPilotDistance 3 bolsterStdDevR <> " km"
                                elN $ "n/a"
                                elD $ ""
                                return ())

                    el "tr" $ do
                        el "td" $ text ""
                        el "td" $ text "Working"
                        elV $ showPilotDistance 3 flownStdDev <> " km"
                        elN $ showPilotDistance 3 flownStdDevN <> " km"
                        elD $ ""
                        return ()

                    el "tr" $ do
                        el "td" $ text ""
                        el "th" $ text ""
                        el "th" $ text "Stop Validity"
                        elV $ Vy.showStopValidity sv
                        elN $ Vy.showStopValidity svN
                        elD $ Vy.showStopValidityDiff svN sv
                        return ()

                let tdFoot = elAttr "td" ("colspan" =: "6")
                let foot = el "tr" . tdFoot . text

                el "tfoot" $ do
                    foot "μ Mean."
                    foot "σ Standard Deviation."
                    foot "† Reach as small as actually flown."
                    foot "‡ Reach bolstered below to be no smaller than minimum distance."
                    foot "‖ Extra altitude above goal converted to extra reach via glide."
                    return ()

            elAttr
                "div"
                ("id" =: "stop-working")
                (text "")

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
                        elAttr "td" ("rowspan" =: "4") $ text ""

                        return ()

                    el "tr" $ do
                        el "th" $ text "max"
                        elClass "td" "td-plot-reach" . f
                            $ maximum rs
                        elClass "td" "td-plot-reach-bonus" . f
                            $ maximum bs
                        elClass "td" "td-plot-reach-bonus-diff" . f
                            $ maximum ds

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
