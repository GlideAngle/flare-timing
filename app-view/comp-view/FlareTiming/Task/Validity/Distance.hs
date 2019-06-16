module FlareTiming.Task.Validity.Distance
    ( viewDistance
    , distanceWorking
    ) where

import Prelude hiding (sum)
import Text.Printf (printf)
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
    , SumOfDistance(..)
    , PilotsFlying(..)
    , showPilotsFlyingDiff
    , showNominalGoal, showNominalGoalDiff
    , showSumOfDistance, showSumOfDistanceDiff
    , showNominalDistance, showNominalDistanceDiff
    , showNominalDistanceArea, showNominalDistanceAreaDiff
    , showMinimumDistance, showMinimumDistanceDiff
    , showMaximumDistance, showMaximumDistanceDiff
    )
import FlareTiming.Task.Validity.Widget (ElementId, spacer, elV, elN, elD)
import FlareTiming.Katex (katexNewLine)

distanceWorkingSubA :: DistanceValidityWorking -> T.Text
distanceWorkingSubA
    DistanceValidityWorking
        { nominalGoal = ng'@(NominalGoal ng)
        , nominalDistance = nd'@(NominalDistance nd)
        , minimumDistance = md'@(MinimumDistance md)
        } =
    " &= ("
    <> (T.pack . show $ ng')
    <> " + 1) * ("
    <> (T.pack . show $ nd')
    <> " - "
    <> (T.pack . show $ md')
    <> ")"
    <> katexNewLine
    <> " &= "
    <> (T.pack . ppr $ ng + 1)
    <> " * "
    <> (T.pack . ppr $ nd - md)
    <> katexNewLine
    <> " &= "
    <> (T.pack . ppr $ (ng + 1) * (nd - md))
    where
        ppr = printf "%.3f"

distanceWorkingSubB :: DistanceValidityWorking -> T.Text
distanceWorkingSubB
    DistanceValidityWorking
        { nominalGoal = ng'@(NominalGoal ng)
        , bestDistance = bd'@(MaximumDistance bd)
        , nominalDistance = nd'@(NominalDistance nd)
        } =
    " &= \\\\max(0, "
    <> (T.pack . show $ ng')
    <> " * ("
    <> (T.pack . show $ bd')
    <> " - "
    <> (T.pack . show $ nd')
    <> "))"
    <> katexNewLine
    <> " &= \\\\max(0, "
    <> (T.pack . show $ ng)
    <> " * "
    <> (T.pack . ppr $ bd - nd)
    <> ")"
    <> katexNewLine
    <> " &= \\\\max(0, "
    <> (T.pack . ppr $ ng * (bd - nd))
    <> ")"
    <> katexNewLine
    <> " &= "
    <> (T.pack . ppr $ max 0 (ng * (bd - nd)))
    where
        ppr 0 = "0"
        ppr x = printf "%.3f" x

distanceWorkingSubArea :: DistanceValidityWorking -> T.Text
distanceWorkingSubArea
    DistanceValidityWorking
        { nominalGoal = NominalGoal ng
        , bestDistance = MaximumDistance bd
        , nominalDistance = NominalDistance nd
        , minimumDistance = MinimumDistance md
        } =
    " = \\\\frac{("
    <> (T.pack $ ppr a)
    <> " + "
    <> (T.pack $ ppr b)
    <> ")}{2}"
    <> " = "
    <> (T.pack . ppr $ (a + b) / 2)
    where
        a = (ng + 1) * (nd - md)
        b = max 0 $ ng * (bd - nd)

        ppr 0 = "0"
        ppr x = printf "%.3f" x

distanceWorkingSubValidity :: DistanceValidityWorking -> T.Text
distanceWorkingSubValidity
    DistanceValidityWorking
        { sum = sum'@(SumOfDistance sd)
        , flying = flying'@(PilotsFlying pf)
        , area = area'@(NominalDistanceArea area)
        } =
    " = \\\\min(1, \\\\frac{"
    <> (T.pack $ show sum')
    <> "}{"
    <> (T.pack $ show flying')
    <> " * "
    <> (showNominalDistanceArea area')
    <> "})"
    <> " = \\\\min(1, "
    <> (T.pack . ppr $ sd / (fromIntegral pf * area))
    <> ")"
    where
        ppr 0 = "0"
        ppr x = printf "%.3f" x

distanceWorking
    :: ElementId
    -> Vy.Validity
    -> DistanceValidityWorking
    -> T.Text
distanceWorking elId v w =
    "katex.render("
    <> "\"\\\\begin{aligned} "
    <> " d_p &= \\\\text{distance flown by pilot \\\\textit{p}}"
    <> katexNewLine
    <> " sum"
    <> " &="
    <> " \\\\sum_p \\\\max(0, d_p - md)"
    <> " = "
    <> (T.pack . show $ sum w)
    <> katexNewLine
    <> katexNewLine
    <> " a &= (ng + 1) * (nd - md)"
    <> katexNewLine
    <> distanceWorkingSubA w
    <> katexNewLine
    <> katexNewLine
    <> " b &= \\\\max(0, ng * (bd - nd))"
    <> katexNewLine
    <> distanceWorkingSubB w
    <> katexNewLine
    <> katexNewLine
    <> "area &= \\\\frac{(a + b)}{2}"
    <> distanceWorkingSubArea w
    <> katexNewLine
    <> katexNewLine
    <> " validity &= \\\\min(1, \\\\frac{sum}{f * area})"
    <> distanceWorkingSubValidity w
    <> " = "
    <> (Vy.showDistanceValidity . Vy.distance $ v)
    <> " \\\\end{aligned}\""
    <> ", getElementById('" <> elId <> "')"
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
                , area = areaN
                , nominalGoal = ngN
                , nominalDistance = ndN
                , minimumDistance = mdN
                , bestDistance = bdN
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

    elAttr "div" ("id" =: "distance-working") $ text ""
    spacer
    elAttr "div" ("id" =: "distance-working-norm") $ text ""

    return ()
