module FlareTiming.Validity.Distance
    ( viewDistance
    , distanceWorking
    ) where

import Prelude hiding (sum)
import Reflex.Dom
import qualified Data.Text as T (Text, pack)

import qualified WireTypes.Validity as Vy
    ( Validity(..)
    , DistanceValidity(..)
    , showDistanceValidity, showDistanceValidityDiff
    )
import WireTypes.ValidityWorking
    ( ValidityWorking(..)
    , DistanceValidityWorking(..)
    , NominalGoal(..)
    , MinimumDistance(..)
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
    )
import WireTypes.Point
    ( PilotDistance(..), ReachToggle(..)
    , showPilotDistance, showPilotDistanceDiff
    )
import FlareTiming.Validity.Widget (ElementId, elV, elN, elD)
import FlareTiming.Katex (Expect(..), Recalc(..), ppr, katexNewLine, katexCheck)

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

distanceWorkingSubB :: DistanceValidityWorking -> T.Text
distanceWorkingSubB
    DistanceValidityWorking
        { nominalGoal = ng'@(NominalGoal ng)
        , nominalDistance = nd'@(NominalDistance nd)
        , reachMax = ReachToggle{flown = bd'@(PilotDistance bd)}
        } =
    " &= \\\\max(0, "
    <> (T.pack . show $ ng')
    <> " * ("
    <> (showPilotDistance 3 bd' <> "km")
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

distanceWorkingSubArea :: DistanceValidityWorking -> T.Text
distanceWorkingSubArea
    DistanceValidityWorking
        { nominalGoal = NominalGoal ng
        , nominalDistance = NominalDistance nd
        , minimumDistance = MinimumDistance md
        , reachMax = ReachToggle{flown = PilotDistance bd}
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

distanceWorkingSubValidity :: Vy.Validity -> DistanceValidityWorking -> T.Text
distanceWorkingSubValidity
    Vy.Validity{distance = Vy.DistanceValidity dv}
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
    <> (T.pack $ ppr dvUnbound)
    <> ")"
    <> " = "
    <> (T.pack $ ppr dv')
    <> katexCheck 3 (Recalc dv') (Expect dv)
    where
        dvUnbound = sd / (fromIntegral pf * area)
        dv' = min 1 dvUnbound

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
    <> distanceWorkingSubValidity v w
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
                , reachMax =
                    ReachToggle
                        { flown = bdF
                        , extra = bdE
                        }
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
                el "td" $ text "Max Flown Bolster (Best Distance)"
                elV $ showPilotDistance 3 bdF <> "km"
                elN $ showPilotDistance 3 bdFN <> "km"
                elD $ showPilotDistanceDiff 3 bdFN bdF
                return ()

            el "tr" $ do
                el "td" $ text ""
                el "td" $ text "Max Extra Bolster"
                elV $ showPilotDistance 3 bdE <> "km"
                elN $ showPilotDistance 3 bdEN <> "km"
                elD $ showPilotDistanceDiff 3 bdEN bdE
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
    elAttr "div" ("id" =: "distance-working-norm") $ text ""

    return ()
