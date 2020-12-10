module FlareTiming.Score.Show
    ( showSpeedSection
    , showMax
    , showRank
    , showSs
    , showGs
    , showEs
    , showPilotTime
    , showPilotTimeDiff
    , showSsVelocityTime
    , showGsVelocityTime
    , showVelocityVelocity
    ) where

import Text.Printf (printf)
import Reflex.Dom
import qualified Data.Text as T (Text, pack)
import Data.Time.LocalTime (TimeZone)

import WireTypes.Route (TaskLength(..), showTaskDistance)
import qualified WireTypes.Point as Pt (StartGate(..))
import WireTypes.Speed (PilotTime(..))
import WireTypes.Point (TaskPlacing(..), Velocity(..), PilotVelocity(..))
import FlareTiming.Time
    (showHmsForHours, showT, showSignedSecs, hoursToSecs, hoursToRoundSecs)

showSpeedSection :: Maybe TaskLength -> T.Text
showSpeedSection =
    T.pack
    . maybe
        "Speed Section"
        (\TaskLength{..} ->
            let tr = showTaskDistance taskRoute
                ss = showTaskDistance taskRouteSpeedSubset
            in printf "Speed Section (%s of %s)" ss tr)

showMax
    :: (Reflex t, Functor f)
    => (a -> b)
    -> (f b -> b -> c)
    -> Dynamic t (f a)
    -> Dynamic t a
    -> Dynamic t c
showMax getField f pt points =
    zipDynWith
        f
        ((fmap . fmap) getField pt)
        (getField <$> points)

showRank :: TaskPlacing -> T.Text
showRank (TaskPlacing p) = T.pack . show $ p
showRank (TaskPlacingEqual p) = T.pack $ show p ++ "="

showSs :: TimeZone -> Velocity -> T.Text
showSs tz Velocity{ss = Just t} = showT tz t
showSs _ _ = ""

showGs :: TimeZone -> Velocity -> T.Text
showGs tz Velocity{gs = Just (Pt.StartGate t)} = showT tz t
showGs _ _ = ""

showEs :: TimeZone -> Velocity -> T.Text
showEs tz Velocity{es = Just t} = showT tz t
showEs _ _ = ""

showPilotTime :: PilotTime -> T.Text
showPilotTime (PilotTime t) = showHmsForHours t

showPilotTimeDiff :: PilotTime -> PilotTime -> T.Text
showPilotTimeDiff (PilotTime expected) (PilotTime actual)
    | actual == expected = "="
    | hoursToRoundSecs actual == hoursToRoundSecs expected = "="
    | abs secs < 1 = showSignedSecs secs
    | hrs < 0 = "-" <> showHmsForHours (negate hrs)
    | otherwise = "+" <> showHmsForHours hrs
        where
            hrs = actual - expected
            secs = hoursToSecs hrs

showSsVelocityTime :: Velocity -> T.Text
showSsVelocityTime Velocity{ssElapsed = Just (PilotTime t)} =
    showHmsForHours t

showSsVelocityTime _ = ""

showGsVelocityTime :: Velocity -> T.Text
showGsVelocityTime Velocity{gsElapsed = Just (PilotTime t)} =
    showHmsForHours t

showGsVelocityTime _ = ""

showVelocityVelocity :: Velocity -> T.Text
showVelocityVelocity Velocity{gsVelocity = Just (PilotVelocity v)} =
    T.pack . printf "%.1f" $ v
showVelocityVelocity _ = ""
