{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module FlareTiming.Plot.Event where

import Text.Printf (printf)
import Reflex.Dom
import Reflex.Time (delay)
import Data.Text (Text)
import qualified Data.Text as T (length, pack, unpack)
import Control.Monad (when)
import Control.Monad.Fix (MonadFix(..))
import Control.Monad.IO.Class (MonadIO(..))
import GHCJS.Types (JSVal)
import GHCJS.DOM.Types (ToJSVal(..), toJSValListOf)

import WireTypes.Pilot (Pilot(..), nullPilot)
import FlareTiming.Pilot (showPilot)

uncurry5 :: (a -> b -> c -> d -> e -> f) -> ((a, b, c, d, e) -> f)
uncurry5 f ~(a, b, c, d, e) = f a b c d e

unpackSelect :: ToJSVal a => [a] -> IO (JSVal, JSVal, JSVal, JSVal, JSVal)
unpackSelect ys = do
    ys1 <- toJSValListOf $ take 1 ys
    ys2 <- toJSValListOf $ take 1 $ drop 1 ys
    ys3 <- toJSValListOf $ take 1 $ drop 2 ys
    ys4 <- toJSValListOf $ take 1 $ drop 3 ys
    ys5 <- toJSValListOf $ take 1 $ drop 4 ys
    return (ys1, ys2, ys3, ys4, ys5)

tableClass :: Pilot -> Text
tableClass p = ("legend table" :: Text) <> if p == nullPilot then " is-hidden" else ""

rowClass :: (Foldable t, Eq a) => a -> t a -> Text
rowClass p ps = if p `elem` ps then "is-selected" else ""

mkMsg :: MonadWidget t m => Dynamic t Pilot -> Text -> m ()
mkMsg dPilot msg = do
    let dMsgClass = ffor dPilot (\p -> "message is-primary" <> if p == nullPilot then "" else " is-hidden")

    _ <- elDynClass "article" dMsgClass $ do
            elClass "div" "message-header" $ do
                el "p" $ text "Plot Instructions"
            elClass "div" "message-body" $
                text msg

    return ()

trimRightPilot :: Int -> Text -> Text
trimRightPilot n name =
    if T.length name > n
        then T.pack . printf "%s ..." . take n $ T.unpack name
        else name

mkLegend :: MonadWidget t m => Dynamic t Int -> Text -> Pilot -> m ()
mkLegend w classes pp = when (pp /= nullPilot) $ do
    el "tr" $ do
        el "td" $ elClass "span" classes $ text "▩"
        el "td" . dynText $ ffor w (trimRightPilot 20 . (flip showPilot) pp)
        return ()

legendClasses :: [Text]
legendClasses = fmap ("legend-" <>) ["reach", "effort", "time", "leading", "arrival"]

numLegendPilots :: Int
numLegendPilots = length legendClasses

selectPilots
    :: (MonadIO (Performable m), PostBuild t m, TriggerEvent t m, PerformEvent t m, MonadHold t m, Control.Monad.Fix.MonadFix m)
    => Dynamic t [Pilot]
    -> (Dynamic t [Pilot] -> m (Event t Pilot))
    -> m (Dynamic t Pilot, Event t [Pilot], (Event t Pilot, Event t Pilot, Event t Pilot, Event t Pilot, Event t Pilot))
selectPilots dPilots x = do
    pb <- delay 1 =<< getPostBuild

    ePilot :: Event _ Pilot <- x dPilots
    dPilot :: Dynamic _ Pilot <- holdDyn nullPilot ePilot

    let es :: Event _ [Pilot] = updated dPilots
    let eRedraw = leftmost [[] <$ pb, es]

    let nth n = updated
                <$> foldDyn
                        (\ps np ->
                            case take 1 . drop n $ (ps ++ repeat np) of
                                p : _ -> p
                                _ -> np)
                        nullPilot
                        es

    [e1, e2, e3, e4, e5] <- sequence $ nth <$> [0 .. 4]

    -- TODO: Find out why I get "cyclic evaluation in fixIO" if I pass a list
    -- rather than 5-tuple for the legend events with selected pilots.
    return (dPilot, eRedraw, (e1, e2, e3, e4, e5))
