module FlareTiming.Validity.Time
    ( viewTime
    , timeWorking
    ) where

import Reflex.Dom
import Data.String (IsString)
import Text.Printf (printf)
import qualified Data.Text as T (Text, pack)

import qualified WireTypes.Validity as Vy
    ( Validity(..)
    , TimeValidity(..)
    , showTimeValidity, showTimeValidityDiff
    )
import WireTypes.ValidityWorking
    ( ValidityWorking(..)
    , TimeValidityWorking(..)
    , BestTime(..)
    , NominalTime(..)
    , NominalDistance(..)
    , showBestTime, showBestTimeDiff
    , showNominalTime, showNominalTimeDiff
    , showNominalDistance, showNominalDistanceDiff
    )
import WireTypes.Point
    ( PilotDistance(..), ReachToggle(..)
    , showPilotDistance, showPilotDistanceDiff
    )
import FlareTiming.Validity.Widget (ElementId, elV, elN, elD)
import FlareTiming.Katex (Expect(..), Recalc(..), ppr, katexNewLine, katexCheck)

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
    TimeValidityWorking
        { nominalDistance = nd
        , reachMax = ReachToggle{extra = bd}
        } =
    " &="
    <> " \\\\dfrac{"
    <> (showPilotDistance 3 bd <> "km")
    <> "}{"
    <> (T.pack . show $ nd)
    <> "}"

timeWorking :: ElementId -> Vy.Validity -> TimeValidityWorking -> T.Text
timeWorking
    elId
    Vy.Validity{time = Vy.TimeValidity tv}
    w@TimeValidityWorking
        { gsBestTime = bt
        , nominalTime = NominalTime nt
        , nominalDistance = NominalDistance nd
        , reachMax = ReachToggle{extra = PilotDistance bd}
        } =
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
    <> katexNewLine
    <> timeWorkingCase bt
    <> katexNewLine
    <> timeWorkingSub w
    <> katexNewLine
    <> katexNewLine
    <> " y &= \\\\min(1, x)"
    <> " = \\\\min(1, "
    <> (T.pack $ ppr x)
    <> ")"
    <> " = "
    <> (T.pack $ ppr y)
    <> katexNewLine
    <> katexNewLine
    <> " z &= -0.271 + 2.912 * y - 2.098 * y^2 + 0.457 * y^3"
    <> " = "
    <> (T.pack $ ppr z)
    <> katexNewLine
    <> katexNewLine
    <> "validity &= \\\\max(0, \\\\min(1, z))"
    <> " = "
    <> (T.pack $ ppr tv')
    <> katexCheck 3 (Recalc tv') (Expect tv)
    <> " \\\\end{aligned}\""
    <> ", getElementById('" <> elId <> "')"
    <> ", {throwOnError: false});"
    where
        x = maybe (bd / nd) (\(BestTime bt') -> bt' / nt) bt
        y = min 1 x
        z = -0.271 + 2.912 * y - 2.098 * y**2 + 0.457 * y**3
        tv' = max 0 $ min 1 z

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
                , nominalDistance = nd
                , reachMax =
                    ReachToggle
                        { flown = bdF
                        , extra = bdE
                        }
                }
        }
    ValidityWorking
        { time =
            TimeValidityWorking
                { gsBestTime = btN
                , nominalTime = ntN
                , nominalDistance = ndN
                , reachMax =
                    ReachToggle
                        { flown = bdFN
                        , extra = bdEN
                        }
                }
        }
    = do
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
                el "td" $ text ""
                el "td" $ text "Max Flown Bolster"
                elV $ showPilotDistance 3 bdF <> "km"
                elN $ showPilotDistance 3 bdFN <> "km"
                elD $ showPilotDistanceDiff 3 bdFN bdF
                return ()

            el "tr" $ do
                el "td" $ text "bd"
                el "td" $ text "Max Extra Bolster (Best Distance)"
                elV $ showPilotDistance 3 bdE <> "km"
                elN $ showPilotDistance 3 bdEN <> "km"
                elD $ showPilotDistanceDiff 3 bdEN bdE
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

    elAttr "div" ("id" =: "time-working") $ text ""
    elAttr "div" ("id" =: "time-working-norm") $ text ""

    return ()
