module FlareTiming.Time
    ( showHmsForHours
    , showHours
    , showT
    , showTDiff
    , timeZone
    ) where

import Prelude hiding (min)
import qualified Data.Text as T (Text, pack)
import Text.Printf (printf)
import Data.Time.Clock (UTCTime, diffUTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.LocalTime (TimeZone, minutesToTimeZone, utcToLocalTime)
import WireTypes.Comp (UtcOffset(..))

show2i :: Integer -> String
show2i = printf "%02d"

showHours :: Double -> T.Text
showHours hr =
    T.pack $ printf "%.03f" hr

showHmsForHours :: Double -> T.Text
showHmsForHours hr =
    T.pack $ show2i hr' ++ ":" ++ show2i min' ++ ":" ++ show2i sec'
    where
        sec = round $ 3600 * abs hr
        (hr', min) = sec `divMod` 3600
        (min', sec') = min `divMod` 60

showT :: TimeZone -> UTCTime -> T.Text
showT tz =
    T.pack
    . formatTime defaultTimeLocale "%T"
    . utcToLocalTime tz

showTDiff :: UTCTime -> UTCTime -> T.Text
showTDiff expected actual =
    if | expected == actual -> "="
       | hrs < 0 -> "-" <> showHmsForHours (negate hrs)
       | otherwise -> "+" <> showHmsForHours hrs
    where
        secs :: Integer
        secs = round $ actual `diffUTCTime` expected

        hrs = fromIntegral secs / 3600.0

timeZone :: UtcOffset -> TimeZone
timeZone UtcOffset{timeZoneMinutes = tzMins} = minutesToTimeZone tzMins
