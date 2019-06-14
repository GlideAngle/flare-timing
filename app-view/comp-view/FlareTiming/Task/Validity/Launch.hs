module FlareTiming.Task.Validity.Launch
    ( viewLaunch
    , launchWorking
    ) where

import Reflex.Dom
import qualified Data.Text as T (Text, pack)

import qualified WireTypes.Validity as Vy
    ( Validity(..)
    , showLaunchValidity, showLaunchValidityDiff
    )
import WireTypes.ValidityWorking
    ( ValidityWorking(..)
    , LaunchValidityWorking(..)
    , showPilotsPresentDiff
    , showPilotsFlyingDiff
    , showNominalLaunchDiff
    )
import FlareTiming.Task.Validity.Widget (katexNewLine, elV, elN, elD)

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

    elAttr
        "div"
        ("id" =: "launch-working")
        (text "")

    return ()


