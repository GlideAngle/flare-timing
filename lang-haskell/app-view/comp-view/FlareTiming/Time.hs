module FlareTiming.Time
    ( UtcOffset(..)
    , showHmsForSecs
    , showHmsForHours
    , hoursToSecs
    , hoursToRoundSecs
    , showHours
    , showSignedSecs
    , showTimePico
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
import GHC.Generics (Generic)
import Data.Aeson (FromJSON(..))

newtype UtcOffset = UtcOffset { timeZoneMinutes :: Int }
    deriving (Eq, Ord, Show, Read, Generic)
    deriving anyclass (FromJSON)

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

showHmsForSecs :: Double -> T.Text
showHmsForSecs s =
    T.pack $ if hms == "00:00:00" then hms' else hms

    where
        hms = show2i hr' ++ ":" ++ show2i min' ++ ":" ++ show2i sec'
        sec = round $ abs s
        (hr', min) = sec `divMod` 3600
        (min', sec') = min `divMod` 60
        s' = s - fromIntegral ((hr' * 3600) + (min' * 60))
        hms' = show2i hr' ++ ":" ++ show2i min' ++ ":" ++ printf "%06.3f" s'

showHmsForHours :: Double -> T.Text
showHmsForHours = showHmsForSecs . hoursToSecs

showTimePico :: TimeZone -> UTCTime -> String
showTimePico tz =
    formatTime defaultTimeLocale "%T %q"
    . utcToLocalTime tz

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
