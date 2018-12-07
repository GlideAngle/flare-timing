module FlareTiming.Time (showHmsForHours) where

import Prelude hiding (min)
import qualified Data.Text as T (Text, pack, unpack, breakOn)
import Text.Printf (printf)

show2i :: Integer -> String
show2i = printf "%02d"

showHmsForHours :: T.Text -> T.Text
showHmsForHours t =
    T.pack $ show2i hr' ++ ":" ++ show2i min' ++ ":" ++ show2i sec'
    where
        hrStr = T.unpack . fst . T.breakOn " h" $ t
        hr = read hrStr :: Double
        sec = round $ 3600 * hr
        (hr', min) = sec `divMod` 3600
        (min', sec') = min `divMod` 60
