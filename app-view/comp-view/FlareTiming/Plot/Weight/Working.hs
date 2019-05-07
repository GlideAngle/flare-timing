module FlareTiming.Plot.Weight.Working (viewWeightWorking) where

import Prelude hiding (sum)
import Reflex
import Reflex.Dom
import Data.String (IsString)
import Text.Printf (printf)
import qualified Data.Text as T (Text, pack)

import WireTypes.Comp (Discipline(..), Tweak(..))
import WireTypes.Point (Allocation(..))

katexNewLine :: T.Text
katexNewLine = " \\\\\\\\ "

spacer :: DomBuilder t m => m ()
spacer = elClass "div" "spacer" $ return ()

hookWorking :: Tweak -> Allocation -> T.Text
hookWorking tweak alloc =
    "katex.render("
    <> "\"\\\\begin{aligned} "
    <> " gr &= \\\\frac{pg}{pf}"
    <> katexNewLine
    <> katexNewLine
    <> " dw &= 0.9 - 1.665 * gr + 1.713 * gr^2 - 0.587 * gr^3"
    <> katexNewLine
    <> " lw &= \\\\frac{1 - dw}{8} * 1.4"
    <> katexNewLine
    <> " aw &= \\\\frac{1 - dw}{8}"
    <> katexNewLine
    <> " tw &= 1 - dw - lw - aw"
    <> katexNewLine
    <> katexNewLine
    <> " p &= 1000 * tv"
    <> katexNewLine
    <> katexNewLine
    <> " dp &= p * dw"
    <> katexNewLine
    <> " lp &= p * lw"
    <> katexNewLine
    <> " ap &= p * aw"
    <> katexNewLine
    <> " tp &= p * tw"
    <> katexNewLine
    <> " \\\\end{aligned}\""
    <> ", getElementById('alloc-working')"
    <> ", {throwOnError: false});"

viewWeightWorking
    :: MonadWidget t m
    => Discipline
    -> Dynamic t (Maybe Tweak)
    -> Dynamic t (Maybe Allocation)
    -> m ()
viewWeightWorking hgOrPg tweak alloc = do
    _ <- dyn $ ffor2 tweak alloc (\x y ->
        case (x, y) of
            (Nothing, _) -> text "Loading competition tweaks ..."
            (_, Nothing) -> text "Loading allocations ..."
            (Just x', Just y') -> do
                elAttr
                    "a"
                    (("class" =: "button") <> ("onclick" =: hookWorking x' y'))
                    (text "Show Working")

                elAttr
                    "div"
                    ("id" =: "alloc-working")
                    (text "")

                return ())

    return ()
