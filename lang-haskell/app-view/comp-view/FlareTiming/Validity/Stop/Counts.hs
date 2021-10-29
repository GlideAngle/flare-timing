module FlareTiming.Validity.Stop.Counts (viewStopCounts) where

import Prelude hiding (sum)
import Reflex.Dom
import qualified Data.Text as T (pack)
import Control.Monad.Zip (mzip)

import qualified WireTypes.Validity as Vy
    ( Validity(..)
    , showStopValidity, showStopValidityDiff
    )
import WireTypes.ValidityWorking
    ( ValidityWorking(..)
    , StopValidityWorking(..)
    , showPilotsFlyingDiff
    , showPilotsLandedDiff
    , showPilotsAtEssDiff
    , showLaunchToEss, showLaunchToEssDiff
    )
import FlareTiming.Validity.Widget (elV, elN, elD)

viewStopCounts
    :: MonadWidget t m
    => Vy.Validity
    -> Vy.Validity
    -> ValidityWorking
    -> ValidityWorking
    -> m ()
viewStopCounts Vy.Validity{stop = Nothing} _ _ _ = return ()
viewStopCounts _ Vy.Validity{stop = Nothing} _ _ = return ()
viewStopCounts _ _ ValidityWorking{stop = Nothing} _ = return ()
viewStopCounts _ _ _ ValidityWorking{stop = Nothing} = return ()
viewStopCounts
    Vy.Validity{stop = sv}
    Vy.Validity{stop = svN}
    -- | Working from flare-timing.
    ValidityWorking
        { stop =
            Just StopValidityWorking
                { pilotsAtEss
                , flying
                , landed
                , launchToEssDistance = ed
                }
        }
    -- | Working from FS, normal or expected.
    ValidityWorking
        { stop =
            Just StopValidityWorking
                { pilotsAtEss = pilotsAtEssN
                , flying = flyingN
                , landed = landedN
                , launchToEssDistance = edN
                }
        }
    = do

    elClass "table" "table is-striped" $ do
        el "thead" $ do
            el "tr" $ do
                elAttr "th" ("colspan" =: "2") $ text ""
                elClass "th" "has-text-right" $ text "Pilots"
                elClass "th" "has-text-right" $ text "Distance"
                elClass "th" "has-text-right" $ text "Validity"
                elClass "th" "th-norm validity" $ text "✓"
                elClass "th" "th-norm th-diff" $ text "Δ"

        el "tbody" $ do
            el "tr" $ do
                el "td" $ text ""
                el "td" $ text "at ESS"
                elV . T.pack $ show pilotsAtEss
                el "td" $ text ""
                el "td" $ text ""
                elN . T.pack $ show pilotsAtEssN
                elD $ showPilotsAtEssDiff pilotsAtEssN pilotsAtEss
                return ()

            el "tr" $ do
                el "td" $ text "f"
                el "td" $ text "Flying"
                elV . T.pack $ show flying
                el "td" $ text ""
                el "td" $ text ""
                elN . T.pack $ show flyingN
                elD $ showPilotsFlyingDiff flyingN flying
                return ()

            el "tr" $ do
                el "td" $ text "ls"
                el "td" $ text "Landed before Stop"
                elV . T.pack $ show landed
                el "td" $ text ""
                el "td" $ text ""
                elN . T.pack $ show landedN
                elD $ showPilotsLandedDiff landedN landed
                return ()

            el "tr" $ do
                el "td" $ text "ed"
                el "td" $ text "Launch to ESS"
                el "td" $ text ""
                elV $ maybe "" showLaunchToEss ed
                el "td" $ text ""
                elN $ maybe "" showLaunchToEss edN
                elD . maybe "" (uncurry showLaunchToEssDiff) $ mzip edN ed
                return ()

            el "tr" $ do
                el "td" $ text ""
                el "th" $ text "Stop"
                el "td" $ text ""
                el "td" $ text ""
                elV $ Vy.showStopValidity sv
                elN $ Vy.showStopValidity svN
                elD $ Vy.showStopValidityDiff svN sv
                return ()
