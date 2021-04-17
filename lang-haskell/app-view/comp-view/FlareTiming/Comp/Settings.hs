module FlareTiming.Comp.Settings (tableComp) where

import Reflex.Dom
import Text.Printf (printf)
import qualified Data.Text as T (Text, pack)
import Data.Maybe (listToMaybe)

import WireTypes.Comp
    (EarthMath, EarthModel(..), Ellipsoid(..), Comp(..), Nominal(..)
    , Give(..), UtcOffset(..), ScoreBackTime
    , showMinimumDistance, showScoreBackTime
    )
import WireTypes.ZoneKind (Radius(..), showRadius)

tableComp
    :: MonadWidget t m
    => Dynamic t [Nominal]
    -> Dynamic t Comp
    -> m ()
tableComp ns c = do
    let n = listToMaybe <$> ns
    _ <- elClass "table" "table is-bordered" $ do
            el "thead" $
                el "tr" $ do
                    elAttr "th" ("colspan" =: "3") $ text ""
                    el "th" $ text "Value"

            _ <- el "tbody" . dyn $ ffor2 n c compRows
            el "tfoot" $
                el "tr" $
                    elAttr "td" ("colspan" =: "4") $
                        text " * Adjusting the turnpoint radius with some give for pilots just short of the control zone"

    return ()

compRows :: MonadWidget t m => Maybe Nominal -> Comp -> m ()
compRows n c@Comp{..} = do
    nominal n utcOffset scoreBack
    earthModelRows earth
    earthMathRows earthMath
    giveRows c

earthModelRows :: MonadWidget t m => EarthModel -> m ()

earthModelRows (EarthAsSphere r) = el "tr" $ do
    el "th" $ text "Earth model"
    elAttr "td" ("colspan" =: "2")
        $ text "Sphere with radius"
    el "td" . text . T.pack . showRadius $ r

earthModelRows (EarthAsEllipsoid Ellipsoid{..}) = do
    el "tr" $ do
        elAttr "th" ("rowspan" =: "2") $ text "Earth model"
        elAttr "td" ("rowspan" =: "2") $ text "Ellipsoid"
        el "td" $ text "semi-major axis"
        el "td" . text . T.pack . showRadius $ equatorialR 

    el "tr" $ do
        el "td" $ text "reciprocal of flattening"
        el "td" . text . T.pack . printf "%f" $ recipF

earthModelRows (EarthAsFlat p) = el "tr" $ do
    el "th" $ text "Earth model"
    elAttr "td" ("colspan" =: "2") $ text "Flat with projection"
    el "td" . text . T.pack . show $ p

earthMathRows :: MonadWidget t m => EarthMath -> m ()
earthMathRows em = el "tr" $ do
    elAttr "th" ("colspan" =: "3") $ text "Earth math"
    el "td" . text . T.pack . show $ em

giveRows :: MonadWidget t m => Comp -> m ()
giveRows Comp{..} = do
    _ <- case give of
        Just Give{giveFraction = gf, giveDistance = Nothing} -> do
            el "tr" $ do
                el "th" $ text "* Give"
                elAttr "td" ("colspan" =: "2")
                    $ text "give fraction only, no give distance"
                el "td" . text . T.pack . printf "%.5f" $ gf

        Just Give{giveFraction = gf, giveDistance = Just (Radius gd)} -> do
            el "tr" $ do
                elAttr "th" ("rowspan" =: "2" <> "colspan" =: "2") $ text "* Give"
                el "td" $ text "Fraction"
                el "td" . text . T.pack . printf "%.5f" $ gf

            el "tr" $ do
                el "td" $ text "Distance"
                el "td" . text . T.pack . printf "%.3f m" $ gd

        Nothing ->
            elClass "tr" "tr-give" $ do
                elAttr "td" ("colspan" =: "4" <> "class" =: "td-missing")
                    $ text "Scoring without any give * in zone radii"

    return ()

nominal
    :: MonadWidget t m
    => Maybe Nominal
    -> UtcOffset
    -> Maybe ScoreBackTime
    -> m ()
nominal n u sb = do
    case n of
        Nothing -> text "Loading nominals ..."
        Just Nominal{..} -> do
            el "tr" $ do
                elAttr "th" ("colspan" =: "3") $ text "UTC Offset"
                el "td" $ do
                    elClass "span" "tag is-warning" $ do
                        text $ showUtcOffset u

            el "tr" $ do
                elAttr "th" ("colspan" =: "3") $ text "Minimum Distance"
                el "td" $ do
                    elClass "span" "tag is-black" $ do
                        text . showMinimumDistance $ free

            el "tr" $ do
                elAttr "th" ("colspan" =: "3") $ text "Nominal Distance"
                el "td" $ do
                    elClass "span" "tag is-info" $ do
                        text $ T.pack distance

            el "tr" $ do
                elAttr "th" ("colspan" =: "3") $ text "Nominal Time"
                el "td" $ do
                    elClass "span" "tag is-success" $ do
                        text . T.pack . show $ time

            el "tr" $ do
                elAttr "th" ("colspan" =: "3") $ text "Nominal Goal"
                el "td" $ do
                    elClass "span" "tag is-primary" $ do
                        text . T.pack . show $ goal

            case sb of
                Nothing -> return ()
                Just sb' ->
                    el "tr" $ do
                        elAttr "th" ("colspan" =: "3") $ text "Score Back Time"
                        el "td" $ do
                            elClass "span" "tag is-danger" $ do
                                text . T.pack . showScoreBackTime $ sb'

    return ()

showUtcOffset :: UtcOffset -> T.Text
showUtcOffset UtcOffset{timeZoneMinutes = mins} =
    T.pack $ show mins ++ " mins"
