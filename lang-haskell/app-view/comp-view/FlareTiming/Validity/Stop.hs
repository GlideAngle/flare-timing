module FlareTiming.Validity.Stop
    ( viewStop
    , stopWorking
    ) where

import Prelude hiding (max, sum)
import qualified Prelude as Stats (max, sum)
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (TimeZone)
import Reflex
import Reflex.Dom
import Text.Printf (printf)
import qualified Data.Text as T (Text, pack)
import Data.Maybe (fromMaybe)
import Data.Map (Map)
import qualified Data.Map.Strict as Map

import qualified WireTypes.Validity as Vy
    (Validity(..), StopValidity(..), showStopValidity)
import WireTypes.ValidityWorking
    ( ValidityWorking(..)
    , StopValidityWorking(..)
    , TimeValidityWorking(..)
    , ReachStats(..)
    , PilotsFlying(..)
    , PilotsLanded(..)
    , LaunchToEss(..)
    , showLaunchToEss
    )
import WireTypes.Route (TaskLength(..))
import WireTypes.Cross (FlyingSection)
import WireTypes.Reach (TrackReach(..))
import qualified WireTypes.Reach as Stats (BolsterStats(..))
import WireTypes.Point
    ( PilotDistance(..), ReachToggle(..)
    , showPilotDistance, showPilotDistanceDiff
    )
import qualified WireTypes.Point as Alt (AltBreakdown(..))
import WireTypes.Pilot (Pilot(..), DfNoTrack(..))
import WireTypes.Comp (UtcOffset(..), MinimumDistance(..))
import FlareTiming.Pilot (showPilotName, rowDfNtReach)
import FlareTiming.Time (timeZone, showTime)
import qualified FlareTiming.Statistics as Stats (mean, stdDev)
import FlareTiming.Validity.Stop.Counts (viewStopCounts)
import FlareTiming.Validity.Stop.Max (viewStopMax)
import FlareTiming.Validity.Stop.Mean (viewStopMean)
import FlareTiming.Validity.Stop.StdDev (viewStopStdDev)
import FlareTiming.Validity.Widget (ElementId)
import FlareTiming.Katex (Expect(..), Recalc(..), ppr, katexNewLine, katexCheck)

stopWorkingCase :: Vy.StopValidity -> Maybe _bestTime -> Double -> Double -> T.Text
stopWorkingCase _ (Just _) _ _ = " &= 1"
stopWorkingCase (Vy.StopValidity sv) Nothing a b =
    " = \\\\min(1, a + b^3)"
    <> " = \\\\min(1, " <> a' <> " + " <> b' <> ")"
    <> " = min(1, "
    <> (T.pack $ ppr svUnbound)
    <> ")"
    <> " = "
    <> (T.pack $ ppr sv')
    <> katexCheck 3 (Recalc sv') (Expect sv)

    where
        b3 = b**3
        b' = T.pack $ printf "%.3f" b3
        a' = T.pack $ printf "%.3f" a

        svUnbound = a + b3
        sv' = min 1 svUnbound

stopWorkingSubA :: StopValidityWorking -> (Double, T.Text)

stopWorkingSubA
    StopValidityWorking
        {launchToEssDistance = Nothing} = (0, "ed is missing")
stopWorkingSubA
    StopValidityWorking
        {reachStats = ReachToggle{flown = Nothing}} = (0, "flown is missing")
stopWorkingSubA
    StopValidityWorking
        { launchToEssDistance = Just (ed@(LaunchToEss ed'))
        , reachStats =
            ReachToggle
                { flown =
                    Just
                    ReachStats
                        { max = bd@(PilotDistance bd')
                        , mean = mf@(PilotDistance mf')
                        , stdDev = sf@(PilotDistance sf')
                        }
                }
        } =
        (a, eqnA)
    where
        eqnA =
            " = \\\\sqrt{\\\\frac{"
            <> bd''
            <> " - "
            <> mf''
            <> "}{"
            <> ed''
            <> " - "
            <> bd''
            <> " + 1} * \\\\sqrt{\\\\frac{"
            <> sf''
            <> "}{5}}}"
            <> " = \\\\sqrt{\\\\frac{"
            <> (textf $ bd' - mf')
            <> "}{"
            <> (textf $ ed' - bd' + 1)
            <> "} * \\\\sqrt{"
            <> textf (sf' / 5)
            <> "}}"
            <> " = \\\\sqrt{"
            <> x'
            <> " * "
            <> y'
            <> "}"
            <> (" = " <> a')

        bd'' = showPilotDistance 3 bd <> "km"
        ed'' = showLaunchToEss ed
        mf'' = showPilotDistance 3 mf <> "km"
        sf'' = showPilotDistance 3 sf <> "km"

        textf = T.pack . ppr

        x = (bd' - mf') / (ed' - bd' + 1)
        y = sqrt $ sf' / 5
        x' = textf x
        y' = textf y

        a = sqrt $ x * y
        a' = textf a

stopWorkingSubB :: StopValidityWorking -> Double -> T.Text

stopWorkingSubB
    StopValidityWorking
        { flying = PilotsFlying pf
        , landed = PilotsLanded pl
        }
    b' =
    " = \\\\frac{"
    <> ls
    <> "}{"
    <> f
    <> "}"
    <> (" = " <> b)
    where
        ls = T.pack . show $ pl
        f = T.pack . show $ pf
        b = T.pack $ printf "%.3f" b'

stopWorking
    :: ElementId
    -> Vy.Validity
    -> StopValidityWorking
    -> TimeValidityWorking
    -> T.Text
stopWorking _ Vy.Validity{stop = Nothing} _ _ = ""
stopWorking
    elId
    Vy.Validity{stop = Just sv}
    sw@StopValidityWorking
        { flying = PilotsFlying pf
        , landed = PilotsLanded pl
        }
    TimeValidityWorking{gsBestTime = bt} =

    "katex.render("
    <> "\"\\\\begin{aligned} "
    <> " bd &= \\\\max(flown)"
    <> katexNewLine
    <> katexNewLine
    <> " a &= \\\\sqrt{\\\\frac{bd - \\\\mu(flown)}{ed - bd + 1} * \\\\sqrt{\\\\frac{\\\\sigma(flown)}{5}}}"
    <> eqnA
    <> katexNewLine
    <> katexNewLine
    <> " b &= \\\\frac{ls}{f}"
    <> stopWorkingSubB sw b
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
    <> stopWorkingCase sv bt a b
    <> " \\\\end{aligned}\""
    <> ", getElementById('" <> elId <> "')"
    <> ", {throwOnError: false});"
    where
        b = fromIntegral pl / (fromIntegral pf :: Double)
        (a, eqnA) = stopWorkingSubA sw

viewStop
    :: MonadWidget t m
    => Dynamic t UtcOffset
    -> Dynamic t (Maybe TaskLength)
    -> Dynamic t MinimumDistance
    -> Vy.Validity
    -> Vy.Validity
    -> ValidityWorking
    -> ValidityWorking
    -> Stats.BolsterStats
    -> Stats.BolsterStats
    -> Dynamic t [(Pilot, TrackReach)]
    -> Dynamic t [(Pilot, TrackReach)]
    -> Dynamic t [(Pilot, FlyingSection UTCTime)]
    -> Dynamic t [(Pilot, FlyingSection UTCTime)]
    -> Dynamic t DfNoTrack
    -> [(Pilot, Alt.AltBreakdown)]
    -> m ()
viewStop _ _ _ Vy.Validity{stop = Nothing} _ _ _ _ _ _ _ _ _ _ _ = return ()
viewStop _ _ _ _ Vy.Validity{stop = Nothing} _ _ _ _ _ _ _ _ _ _ = return ()
viewStop _ _ _ _ _ ValidityWorking{stop = Nothing} _ _ _ _ _ _ _ _ _ = return ()
viewStop _ _ _ _ _ _ ValidityWorking{stop = Nothing} _ _ _ _ _ _ _ _ = return ()
viewStop
    utc
    ln
    free
    v@Vy.Validity{stop = sv}
    vN
    -- | Working from flare-timing.
    vw
    -- | Working from FS, normal or expected.
    vwN
    -- | Reach as flown.
    sf
    -- | With extra altitude converted by way of glide to extra reach.
    se
    reach
    bonusReach
    landedByStop
    stillFlying
    dfNt'
    sEx = do

    let dfNt = unDfNoTrack <$> dfNt'

    elClass "div" "card" $ do
        elClass "div" "card-content" $ do
            elClass "h2" "title is-4" $ do
                elClass "span" "legend-stop" $ text "▩"
                text $ " Stop Validity = " <> Vy.showStopValidity sv

            elClass "div" "tile is-ancestor" $ do
                elClass "div" "tile is-12" $
                    elClass "div" "tile" $ do
                        elClass "div" "tile is-parent" $ do
                            dyn_ $ ffor dfNt (\dfNt'' ->
                                if null dfNt''
                                    then
                                        elClass "article" "tile is-child notification is-warning" $ do
                                            elClass "p" "title" $ text "DF"
                                            elClass "p" "subtitle" $ text "no track"
                                            el "p" $ text "There are no DF-no-track pilots"
                                    else
                                        elClass "article" "tile is-child box" $ do
                                            elClass "p" "title" $ text "DF"
                                            elClass "p" "subtitle" $ text "no track"
                                            elClass "div" "content" $ do
                                                _ <- elClass "table" "table is-striped is-narrow" $ do
                                                        el "thead" $ do
                                                            el "tr" $ do
                                                                el "th" $ text "#"
                                                                el "th" $ text "Name"
                                                                elClass "th" "th-awarded-reach" $ text "Reach"

                                                        el "tbody" $
                                                            listWithKey
                                                                (Map.fromList . zip [1..] <$> dfNt)
                                                                (rowDfNtReach ln)

                                                el "p" . text
                                                    $ "These pilots get awarded at least minimum distance."
                                )

                        elClass "div" "tile is-parent" $ do
                            elClass "article" "tile is-child box" $ do
                                elClass "p" "title" $ text "Landed"
                                elClass "p" "subtitle" $ text "landed before stop"
                                elClass "div" "content"
                                    $ tablePilotFlyingTimes utc landedByStop

                        elClass "div" "tile is-parent" $ do
                            elClass "article" "tile is-child box" $ do
                                elClass "p" "title" $ text "Flying"
                                elClass "p" "subtitle" $ text "still flying at stop"
                                elClass "div" "content"
                                    $ tablePilotFlyingTimes utc stillFlying

            elClass "div" "tile is-ancestor" $ do
                elClass "div" "tile is-12" $
                    elClass "div" "tile" $
                        elClass "div" "tile is-parent" $
                            elClass "article" "tile is-child box" $ do
                                elClass "p" "title" $ text "Reach"
                                elClass "p" "subtitle" $ text "best distance reached at or before stop"
                                elClass "div" "content"
                                    $ tablePilotReach free reach bonusReach sEx

            elClass "div" "tile is-ancestor" $ do
                elClass "div" "tile is-12" $
                    elClass "div" "tile" $ do
                        elClass "div" "tile is-parent" $ do
                            elClass "article" "tile is-child box" $ do
                                elClass "p" "title" $ text "max"
                                elClass "p" "subtitle" $ text "maximum km"
                                elClass "div" "content" $
                                    viewStopMax vw vwN sf se

                        elClass "div" "tile is-parent" $ do
                            elClass "article" "tile is-child box" $ do
                                elClass "p" "title" $ text "μ"
                                elClass "p" "subtitle" $ text "mean km"
                                elClass "div" "content" $
                                    viewStopMean vw vwN sf se

                        elClass "div" "tile is-parent" $ do
                            elClass "article" "tile is-child box" $ do
                                elClass "p" "title" $ text "σ"
                                elClass "p" "subtitle" $ text "standard deviation km"
                                elClass "div" "content" $
                                    viewStopStdDev vw vwN sf se

            elClass "div" "tile is-ancestor" $ do
                elClass "div" "tile is-12" $
                    elClass "div" "tile" $
                        elClass "div" "tile is-parent" $ do
                            elClass "article" "tile is-child box" $ do
                                elClass "p" "title" $ text "Pilots, Distance & Validity"
                                elClass "p" "subtitle" $ text "other stop validity formula inputs and the stop validity itself"
                                elClass "div" "content" $
                                    viewStopCounts v vN vw vwN

            elAttr "div" ("id" =: "stop-working") $ text ""
            elAttr "div" ("id" =: "stop-working-norm") $ text ""

    return ()

tablePilotFlyingTimes
    :: MonadWidget t m
    => Dynamic t UtcOffset
    -> Dynamic t [(Pilot, FlyingSection UTCTime)]
    -> m ()
tablePilotFlyingTimes utc xs = do
    tz <- sample . current $ timeZone <$> utc
    _ <- elClass "table" "table is-striped" $ do
            el "thead" $ do
                el "tr" $ do
                    el "th" $ text "#"
                    el "th" $ text "Landed"
                    el "th" $ text "Pilot"

                    return ()

            el "tbody" $ do
                listWithKey
                    (Map.fromList . zip [1..] <$> xs)
                    (rowFlyingTimes tz)

    return ()

rowFlyingTimes
    :: MonadWidget t m
    => TimeZone
    -> Int
    -> Dynamic t (Pilot, (FlyingSection UTCTime))
    -> m ()
rowFlyingTimes tz i ptm = do
    let (p, tm) = splitDynPure ptm
    let t = maybe "-" (T.pack . showTime tz . snd) <$> tm
    el "tr" $ do
        el "td" . text . T.pack $ show i
        el "td" $ dynText t
        el "td" . dynText $ showPilotName <$> p

        return ()

tablePilotReach
    :: MonadWidget t m
    => Dynamic t MinimumDistance
    -> Dynamic t [(Pilot, TrackReach)]
    -> Dynamic t [(Pilot, TrackReach)]
    -> [(Pilot, Alt.AltBreakdown)]
    -> m ()
tablePilotReach free reach bonusReach sEx = do
    let tdFoot = elAttr "td" ("colspan" =: "12")
    let foot = el "tr" . tdFoot . text

    _ <- elClass "table" "table is-striped" $ do
            el "thead" $ do
                el "tr" $ do
                    el "th" $ text ""
                    elAttr "th" (("colspan" =: "5") <> ("class" =: "th-valid-reach-col")) $ text "Reach (km)"
                    elAttr "th" (("colspan" =: "7") <> ("class" =: "th-valid-bolster-col"))$ text "Bolster (km)"
                    el "th" $ text ""

                    return ()

            el "thead" $ do
                el "tr" $ do
                    el "th" $ text "#"

                    elClass "th" "th-valid-reach" $ text "Flown †"
                    elClass "th" "th-norm reach" $ text "✓"
                    elClass "th" "th-norm reach-diff" $ text "Δ"

                    elClass "th" "th-valid-reach-extra" $ text "Extra ‡"
                    elClass "th" "th-valid-reach-extra-diff" $ text "Δ"

                    elClass "th" "th-valid-bolster" $ text "Flown"

                    elClass "th" "th-norm bolster" $ text "✓"
                    elClass "th" "th-norm bolster-diff" $ text "Δ"

                    elClass "th" "th-valid-bolster-extra" $ text "Extra"

                    elClass "th" "th-norm bolster-extra" $ text "✓"
                    elClass "th" "th-norm bolster-extra-diff" $ text "Δ"

                    elClass "th" "th-valid-bolster-extra-diff" $ text "Δ"

                    el "th" $ text "Pilot"

                    return ()

            _ <- el "tbody" . dyn $ ffor3 free reach bonusReach (\free'@(MinimumDistance dMin) r br -> do

                    let fOver = Stats.max 0 . (\x -> x - dMin)

                    let rs = [d | (_, TrackReach{reach = PilotDistance d}) <- r]
                    let rsO = fOver <$> rs

                    let rsN =
                            [ maybe 0 (\(PilotDistance d) -> d) flown
                              | (_, Alt.AltBreakdown{reach = ReachToggle{flown}}) <- sEx
                            ]

                    let rsNO = fOver <$> rsN

                    let rsB = Stats.max dMin <$> rs
                    let rsBO = fOver <$> rsB

                    let bs = [d | (_, TrackReach{reach = PilotDistance d}) <- br]
                    let bsO = fOver <$> bs

                    let bsB = Stats.max dMin <$> bs
                    let bsBO = fOver <$> bsB

                    let esN =
                            [ maybe 0 (\(PilotDistance d) -> d) extra
                            | (_, Alt.AltBreakdown{reach = ReachToggle{extra}}) <- sEx
                            ]

                    let esNO = fOver <$> esN

                    let bsN =
                            [ maybe 0 (\(PilotDistance d) -> d) landedMade
                            | (_, Alt.AltBreakdown{landedMade}) <- sEx
                            ]

                    let bsNO = fOver <$> bsN

                    let ds = zipWith (-) bs rs
                    let dsB = zipWith (-) bs rsB

                    let mapR = Map.fromList br
                    let mapN = Map.fromList sEx

                    _ <- listWithKey
                            (Map.fromList . zip [1..] <$> reach)
                            (rowReachBonus free' mapN mapR)

                    let f = text . T.pack . printf "%.3f"
                    let g = text . T.pack . printf "%d"

                    elClass "tr" "tr-sum" $ do
                        el "th" $ text "∑"

                        elClass "td" "td-valid-reach" . f
                            $ Stats.sum rs
                        elClass "th" "th-norm reach" . f
                            $ Stats.sum rsN
                        elClass "th" "th-norm reach-diff" $ text ""

                        elClass "td" "td-valid-reach-extra" . f
                            $ Stats.sum bs
                        elClass "td" "td-valid-reach-extra-diff" . f
                            $ Stats.sum ds

                        elClass "td" "td-valid-bolster" . f
                            $ Stats.sum rsB

                        elClass "td" "td-norm bolster" . f
                            $ Stats.sum bsN
                        elClass "td" "td-norm bolster-diff" $ text ""

                        elClass "td" "td-valid-bolster-extra" . f
                            $ Stats.sum bsB

                        elClass "td" "td-norm bolster-extra" . f
                            $ Stats.sum esN
                        elClass "td" "td-norm bolster-extra-diff" $ text ""

                        elClass "td" "td-valid-bolster-extra-diff" . f
                            $ Stats.sum dsB
                        elAttr "td" ("rowspan" =: "4") $ text ""

                        return ()

                    elClass "tr" "tr-sum" $ do
                        el "th" $ text "∑ over min"

                        elClass "td" "td-valid-reach" . f
                            $ Stats.sum rsO
                        elClass "th" "th-norm reach" . f
                            $ Stats.sum rsNO
                        elClass "th" "th-norm reach-diff" $ text ""

                        elClass "td" "td-valid-reach-extra" . f
                            $ Stats.sum bsO
                        elClass "td" "td-valid-reach-extra-diff" $ text ""

                        elClass "td" "td-valid-bolster" . f
                            $ Stats.sum rsBO

                        elClass "td" "td-norm bolster" . f
                            $ Stats.sum bsNO
                        elClass "td" "td-norm bolster-diff" $ text ""

                        elClass "td" "td-valid-bolster-extra" . f
                            $ Stats.sum bsBO

                        elClass "td" "td-norm bolster-extra" . f
                            $ Stats.sum esNO
                        elClass "td" "td-norm bolster-extra-diff" $ text ""

                        elClass "td" "td-valid-bolster-extra-diff" $ text ""
                        elAttr "td" ("rowspan" =: "4") $ text ""

                        return ()

                    el "tr" $ do
                        el "th" $ text "n"

                        elClass "td" "valid-max td-valid-reach" . g
                            $ length rs
                        elClass "th" "th-norm reach" . g
                            $ length rsN
                        elClass "th" "th-norm reach-diff" $ text ""

                        elClass "td" "valid-max td-valid-reach-extra" . g
                            $ length bs
                        elClass "td" "valid-max td-valid-reach-extra-diff" . g
                            $ length ds

                        elClass "td" "valid-max td-valid-bolster" . g
                            $ length rsB

                        elClass "td" "td-norm bolster" . g
                            $ length bsN
                        elClass "td" "td-norm bolster-diff" $ text ""

                        elClass "td" "valid-max td-valid-bolster-extra" . g
                            $ length bsB

                        elClass "td" "td-norm bolster-extra" . g
                            $ length esN
                        elClass "td" "td-norm bolster-extra-diff" $ text ""

                        elClass "td" "valid-max td-valid-bolster-extra-diff" . g
                            $ length dsB

                        return ()

                    el "tr" $ do
                        el "th" $ text "max"

                        elClass "td" "valid-max td-valid-reach" . f
                            $ maximum rs
                        elClass "th" "th-norm reach" . f
                            $ maximum rsN
                        elClass "th" "th-norm reach-diff" $ text ""

                        elClass "td" "valid-max td-valid-reach-extra" . f
                            $ maximum bs
                        elClass "td" "valid-max td-valid-reach-extra-diff" . f
                            $ maximum ds

                        elClass "td" "valid-max td-valid-bolster" . f
                            $ maximum rsB

                        elClass "td" "td-norm bolster" . f
                            $ maximum bsN
                        elClass "td" "td-norm bolster-diff" $ text ""

                        elClass "td" "valid-max td-valid-bolster-extra" . f
                            $ maximum bsB

                        elClass "td" "td-norm bolster-extra" . f
                            $ maximum esN
                        elClass "td" "td-norm bolster-extra-diff" $ text ""

                        elClass "td" "valid-max td-valid-bolster-extra-diff" . f
                            $ maximum dsB

                        return ()

                    el "tr" $ do
                        el "th" $ text "μ"

                        elClass "td" "valid-μ td-valid-reach" . f
                            $ Stats.mean rs
                        elClass "th" "th-norm reach" . f
                            $ Stats.mean rsN
                        elClass "th" "th-norm reach-diff" $ text ""

                        elClass "td" "valid-μ td-valid-reach-extra" . f
                            $ Stats.mean bs
                        elClass "td" "valid-μ td-valid-reach-extra-diff" . f
                            $ Stats.mean ds

                        elClass "td" "valid-μ td-valid-bolster" . f
                            $ Stats.mean rsB

                        elClass "td" "td-norm bolster" . f
                            $ Stats.mean bsN
                        elClass "td" "td-norm bolster-diff" $ text ""

                        elClass "td" "valid-μ td-valid-bolster-extra" . f
                            $ Stats.mean bsB

                        elClass "td" "td-norm bolster-extra" . f
                            $ Stats.mean esN
                        elClass "td" "td-norm bolster-extra-diff" $ text ""

                        elClass "td" "valid-μ td-valid-bolster-extra-diff" . f
                            $ Stats.mean dsB

                        return ()

                    el "tr" $ do
                        el "th" $ text "σ"

                        elClass "td" "valid-σ td-valid-reach" . f
                            $ Stats.stdDev rs
                        elClass "th" "th-norm reach" . f
                            $ Stats.stdDev rsN
                        elClass "th" "th-norm reach-diff" $ text ""

                        elClass "td" "valid-σ td-valid-reach-extra" . f
                            $ Stats.stdDev bs
                        elClass "td" "valid-σ td-valid-reach-extra-diff" . f
                            $ Stats.stdDev ds

                        elClass "td" "valid-σ td-valid-bolster" . f
                            $ Stats.stdDev rsB

                        elClass "td" "td-norm bolster" . f
                            $ Stats.stdDev bsN
                        elClass "td" "td-norm bolster-diff" $ text ""

                        elClass "td" "valid-σ td-valid-bolster-extra" . f
                            $ Stats.stdDev bsB

                        elClass "td" "td-norm bolster-extra" . f
                            $ Stats.stdDev esN
                        elClass "td" "td-norm bolster-extra-diff" $ text ""

                        elClass "td" "valid-σ td-valid-bolster-extra-diff" . f
                            $ Stats.stdDev dsB

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
    => MinimumDistance
    -> Map Pilot Alt.AltBreakdown
    -> Map Pilot TrackReach
    -> Int
    -> Dynamic t (Pilot, TrackReach)
    -> m ()
rowReachBonus (MinimumDistance dMin) mapN mapR i pr = do
    let (p, r) = splitDynPure pr

    (reachF
        , bolsterF
        , reachE
        , reachN
        , bolsterE
        , reachDiffE
        , reachDiffN
        , bolsterDiffE
        , extraN
        , bolsterN
        , extraDiffN
        , bolsterDiffN
        ) <- sample . current
            $ ffor2 p r (\p' r' ->
                case (Map.lookup p' mapN, Map.lookup p' mapR) of
                    (Just
                        Alt.AltBreakdown
                            { reach =
                                ReachToggle
                                    { extra = extraN
                                    , flown = reachN
                                    }
                            , landedMade = landedMadeN
                            }
                        , Just br) ->
                        let reachE@(PilotDistance dE) = reach br
                            reachF@(PilotDistance dF) = reach r'

                            bolsterE = PilotDistance $ Stats.max dE dMin
                            bolsterF = PilotDistance $ Stats.max dF dMin

                            f = showPilotDistance 3
                            fDiff = showPilotDistanceDiff 3
                            landedMadeN' = fromMaybe (PilotDistance 0) landedMadeN
                        in
                            ( f reachF
                            , f bolsterF
                            , f reachE
                            , maybe "" f reachN
                            , f bolsterE
                            , fDiff reachF reachE
                            , maybe "" (fDiff reachF) reachN
                            , fDiff bolsterF bolsterE
                            , maybe "" f extraN
                            , f landedMadeN'
                            , maybe "" (fDiff bolsterE) extraN
                            , fDiff bolsterF landedMadeN'
                            )

                    _ -> ("", "", "", "", "", "", "", "", "", "", "", ""))

    el "tr" $ do
        el "td" . text . T.pack $ show i

        elClass "td" "td-valid-reach" $ text reachF
        elClass "td" "td-norm reach" $ text reachN
        elClass "td" "td-norm reach-diff" $ text reachDiffN

        elClass "td" "td-valid-reach-extra" $ text reachE
        elClass "td" "td-valid-reach-extra-diff" $ text reachDiffE

        elClass "td" "td-valid-bolster" $ text bolsterF

        elClass "td" "td-norm bolster" $ text bolsterN
        elClass "td" "td-norm bolster-diff" $ text bolsterDiffN

        elClass "td" "td-valid-bolster-extra" $ text bolsterE

        elClass "td" "td-norm bolster-extra" $ text extraN
        elClass "td" "td-norm bolster-extra-diff" $ text extraDiffN

        elClass "td" "td-valid-bolster-extra-diff" $ text bolsterDiffE

        el "td" . dynText $ showPilotName <$> p

        return ()
