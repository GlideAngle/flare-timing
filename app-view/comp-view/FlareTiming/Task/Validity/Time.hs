module FlareTiming.Task.Validity.Time
    ( viewTime
    , timeWorking
    ) where

import Reflex.Dom
import Data.String (IsString)
import Text.Printf (printf)
import qualified Data.Text as T (Text, pack)

import qualified WireTypes.Validity as Vy
    ( Validity(..)
    , showTimeValidity, showTimeValidityDiff
    )
import WireTypes.ValidityWorking
    ( ValidityWorking(..)
    , TimeValidityWorking(..)
    , BestTime(..)
    , showBestTime, showBestTimeDiff
    , showNominalTime, showNominalTimeDiff
    , showBestDistance, showBestDistanceDiff
    , showNominalDistance, showNominalDistanceDiff
    )
import FlareTiming.Task.Validity.Widget (katexNewLine, elV, elN, elD)

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
                        elV $ showBestDistance bd <> " km"
                        elN $ showBestDistance bdN <> " km"
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
