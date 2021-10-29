module FlareTiming.Validity.Launch
    ( viewLaunch
    , launchWorking
    ) where

import Reflex.Dom
import qualified Data.Text as T (Text, pack)

import qualified WireTypes.Validity as Vy
    ( Validity(..)
    , LaunchValidity(..)
    , showLaunchValidity, showLaunchValidityDiff
    )
import WireTypes.ValidityWorking
    ( ValidityWorking(..)
    , LaunchValidityWorking(..)
    , PilotsFlying(..)
    , PilotsPresent(..)
    , NominalLaunch(..)
    , showPilotsPresentDiff
    , showPilotsFlyingDiff
    , showNominalLaunchDiff
    )
import FlareTiming.Validity.Widget (ElementId, elV, elN, elD)
import FlareTiming.Katex (Expect(..), Recalc(..), ppr, katexNewLine, katexCheck)

launchWorking :: ElementId -> Vy.Validity -> LaunchValidityWorking -> T.Text
launchWorking
    elId
    Vy.Validity{launch = Vy.LaunchValidity lv}
    LaunchValidityWorking
        { flying = PilotsFlying pf
        , present = PilotsPresent pp
        , nominalLaunch = NominalLaunch nl
        } =
    "katex.render("
    <> "\"\\\\begin{aligned} "
    <> "x &= \\\\min(1, \\\\frac{f}{p * n})"
    <> " = \\\\min(1, \\\\frac{"
    <> (T.pack $ show pf)
    <> "}{"
    <> (T.pack $ show pp)
    <> " * "
    <> (T.pack $ ppr nl)
    <> "})"
    <> " = \\\\min(1, "
    <> (T.pack $ ppr xUnbound)
    <> ")"
    <> " = "
    <> (T.pack $ ppr x)
    <> katexNewLine
    <> katexNewLine
    <> "validity &= 0.027 * x + 2.917 * x^2 - 1.944 * x^3"
    <> " = "
    <> (T.pack $ ppr lv')
    <> katexCheck 3 (Recalc lv') (Expect lv)
    <> " \\\\end{aligned}\""
    <> ", getElementById('" <> elId <> "')"
    <> ", {throwOnError: false});"
    where
        xUnbound :: Double
        xUnbound = fromIntegral pf / (fromIntegral pp * nl)

        x = min 1 xUnbound
        lv' = 0.027 * x + 2.917 * x**2 - 1.944 * x**3

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

    elAttr "div" ("id" =: "launch-working") $ text ""
    elAttr "div" ("id" =: "launch-working-norm") $ text ""

    return ()


