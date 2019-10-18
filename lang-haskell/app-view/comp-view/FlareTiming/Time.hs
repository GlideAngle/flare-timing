module FlareTiming.Time
    ( showHmsForHours
    , hoursToSecs
    , hoursToRoundSecs
    , showHours
    , showSignedSecs
    , showTime
    , showT
    , showTDiff
    , showNominalTDiff
    , timeZone
    ) where

import Prelude hiding (min)
import qualified Data.Text as T (Text, pack)
import Text.Printf (printf)
import Data.Time.Clock (UTCTime, NominalDiffTime, diffUTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.LocalTime (TimeZone, minutesToTimeZone, utcToLocalTime)
import WireTypes.Comp (UtcOffset(..))

show2i :: Integer -> String
show2i = printf "%02d"

showHours :: Double -> T.Text
showHours hr = T.pack $ printf "%.03f" hr

showSignedSecs :: Double -> T.Text
showSignedSecs s
  | s < 0 = T.pack $ printf "-00:00:0%.3f" (abs s)
  | otherwise = T.pack $ printf "+00:00:0%.3f" s

hoursToSecs :: Double -> Double
hoursToSecs hr = 3600 * hr

hoursToRoundSecs :: Double -> Integer
hoursToRoundSecs = round . hoursToSecs

showHmsForHours :: Double -> T.Text
showHmsForHours hr =
    T.pack $ show2i hr' ++ ":" ++ show2i min' ++ ":" ++ show2i sec'
    where
        sec = round $ 3600 * abs hr
        (hr', min) = sec `divMod` 3600
        (min', sec') = min `divMod` 60

showTime :: TimeZone -> UTCTime -> String
showTime tz =
    formatTime defaultTimeLocale "%T"
    . utcToLocalTime tz

showT :: TimeZone -> UTCTime -> T.Text
showT tz = T.pack . showTime tz

showTDiff :: UTCTime -> UTCTime -> T.Text
showTDiff expected actual = showNominalTDiff $ actual `diffUTCTime` expected

showNominalTDiff :: NominalDiffTime -> T.Text
showNominalTDiff diff =
    if | roundSecs == 0 -> "="
       | abs secs < 1 -> showSignedSecs $ fromRational secs
       | hrs < 0 -> "-" <> showHmsForHours (negate hrs)
       | otherwise -> "+" <> showHmsForHours hrs
    where
        roundSecs :: Integer
        roundSecs = round diff

        secs :: Rational
        secs = toRational diff

        hrs = (fromRational secs) / 3600.0

timeZone :: UtcOffset -> TimeZone
timeZone UtcOffset{timeZoneMinutes = tzMins} = minutesToTimeZone tzMins
