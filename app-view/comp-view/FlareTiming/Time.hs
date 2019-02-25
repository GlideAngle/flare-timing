module FlareTiming.Time
    ( showHmsForHours
    , showT
    , timeZone
    ) where

import Prelude hiding (min)
import qualified Data.Text as T (Text, pack)
import Text.Printf (printf)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.LocalTime (TimeZone, minutesToTimeZone, utcToLocalTime)
import WireTypes.Comp (UtcOffset(..))

show2i :: Integer -> String
show2i = printf "%02d"

showHmsForHours :: Double -> T.Text
showHmsForHours hr =
    T.pack $ show2i hr' ++ ":" ++ show2i min' ++ ":" ++ show2i sec'
    where
        sec = round $ 3600 * hr
        (hr', min) = sec `divMod` 3600
        (min', sec') = min `divMod` 60

showT :: TimeZone -> UTCTime -> T.Text
showT tz = 
    T.pack
    . formatTime defaultTimeLocale "%T"
    . utcToLocalTime tz

timeZone :: UtcOffset -> TimeZone
timeZone UtcOffset{timeZoneMinutes = tzMins} = minutesToTimeZone tzMins
