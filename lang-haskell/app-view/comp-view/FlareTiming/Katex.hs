module FlareTiming.Katex
    ( Expect(..)
    , Recalc(..)
    , katexNewLine
    , katexCheck
    , ppr
    ) where

import qualified Data.Text as T (Text, pack)
import Text.Printf (printf)

import Data.Ratio.Rounding (dpRound)

katexNewLine :: T.Text
katexNewLine = " \\\\\\\\ "

ppr :: Double -> String
ppr 0 = "0"
ppr x = printf "%.3f" x

newtype Expect a = Expect a
newtype Recalc a = Recalc a

katexCheck :: Integer -> Recalc Double -> Expect Double -> T.Text
katexCheck dp (Recalc x') (Expect y') =
    if (printf "%.*f" dp x :: String) == printf "%.*f" dp y
        then " \\\\color{limegreen}\\\\checkmark"
        else " \\\\color{red}\\\\neq " <> (T.pack $ printf "%.*f" dp y)
    where
        -- WARNING: The rounding of printf may not be as you expect.
        -- > printf "%.3f, %.3f" 0.37251 0.3725
        -- 0.373, 0.372
        f :: Double -> Double
        f = fromRational . dpRound dp . toRational

        x = f x'
        y = f y'
