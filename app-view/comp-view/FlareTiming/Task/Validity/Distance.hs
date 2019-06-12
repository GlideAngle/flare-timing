module FlareTiming.Task.Validity.Distance
    ( viewDistance
    , distanceWorking
    ) where

import Prelude hiding (sum)
import Reflex.Dom
import qualified Data.Text as T (Text, pack)

import qualified WireTypes.Validity as Vy
    ( Validity(..)
    , showDistanceValidity, showDistanceValidityDiff
    )
import WireTypes.ValidityWorking
    ( ValidityWorking(..)
    , DistanceValidityWorking(..)
    , NominalGoal(..)
    , MinimumDistance(..)
    , MaximumDistance(..)
    , NominalDistance(..)
    , NominalDistanceArea(..)
    , showPilotsFlyingDiff
    , showNominalGoal, showNominalGoalDiff
    , showSumOfDistance, showSumOfDistanceDiff
    , showNominalDistance, showNominalDistanceDiff
    , showNominalDistanceArea, showNominalDistanceAreaDiff
    , showMinimumDistance, showMinimumDistanceDiff
    , showMaximumDistance, showMaximumDistanceDiff
    )
import FlareTiming.Task.Validity.Widget (katexNewLine, elV, elN, elD)

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
