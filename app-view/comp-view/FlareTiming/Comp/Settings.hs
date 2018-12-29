module FlareTiming.Comp.Settings (tableComp) where

import Reflex.Dom
import Text.Printf (printf)
import qualified Data.Text as T (pack)

import WireTypes.Comp (Comp(..), Give(..))
import WireTypes.ZoneKind (Radius(..))

tableComp
    :: MonadWidget t m
    => Dynamic t Comp
    -> m ()
tableComp c = do
    _ <- elClass "table" "table is-bordered" $ do
            el "thead" $
                el "tr" $ do
                    elAttr "th" ("colspan" =: "2") $ text "Setting"
                    el "th" $ text "Value"

            el "tbody" . dyn $ ffor c compRows

    return ()

compRows
    :: MonadWidget t m
    => Comp
    -> m ()
compRows Comp{..} = do
    case give of
        Just Give{giveFraction = gf, giveDistance = Nothing} -> do
            el "tr" $ do
                elAttr "td" ("colspan" =: "2")
                    $ text "Give fraction only, no give distance"
                el "td" . text . T.pack . printf "%.5f" $ gf
                return ()

        Just Give{giveFraction = gf, giveDistance = Just (Radius gd)} -> do
            el "tr" $ do
                elAttr "td" ("rowspan" =: "2") $ text "Give"
                el "td" $ text "Fraction"
                el "td" . text . T.pack . printf "%.5f" $ gf
                return ()

            el "tr" $ do
                el "td" $ text "Distance"
                el "td" . text . T.pack . printf "%.3f m" $ gd
                return ()

        Nothing ->
            el "tr" $ do
                elAttr "td" ("colspan" =: "3") $ text "Give is missing"
                return ()

    return ()
