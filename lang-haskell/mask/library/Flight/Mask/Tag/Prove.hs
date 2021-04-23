module Flight.Mask.Tag.Prove (keepCrossing, prove, proveCrossing) where

import Prelude hiding (span)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (UTCTime)
import Control.Lens ((^?), element)

import Flight.Units ()
import qualified Flight.Kml as Kml (Fix)
import Flight.Comp (TimePass)
import Flight.Track.Cross (Fix(..), ZoneCross(..))
import Flight.Track.Time (ZoneIdx(..))

import Flight.Mask.Internal.Race ()
import Flight.Mask.Internal.Zone
    ( ZoneEntry(..)
    , ZoneExit(..)
    , Crossing
    , fixFromFix
    )

keepCrossing :: TimePass -> UTCTime -> [Kml.Fix] -> Crossing -> Bool

keepCrossing pass mark0 fixes (Left (ZoneEntry i@(ZoneIdx i') j@(ZoneIdx j'))) =
    fromMaybe False $ do
        fixM <- fixes ^? element i'
        fixN <- fixes ^? element j'
        let f = fixFromFix mark0
        let [Fix{time = ti}, Fix{time = tj}] = [f i fixM, f j fixN]
        return $ pass ti || pass tj

keepCrossing pass mark0 fixes (Right (ZoneExit i@(ZoneIdx i') j@(ZoneIdx j'))) =
    fromMaybe False $ do
        fixM <- fixes ^? element i'
        fixN <- fixes ^? element j'
        let f = fixFromFix mark0
        let [Fix{time = ti}, Fix{time = tj}] = [f i fixM, f j fixN]
        return $ pass ti || pass tj

-- | Prove from the fixes and mark that the crossing exits. We don't know the
-- interpolated crossing point and time yet so we'll accept a crossing where
-- one of the fixes passes the time check.
prove
    :: TimePass
    -> UTCTime
    -> [Kml.Fix]
    -> ZoneIdx
    -> ZoneIdx
    -> [Bool]
    -> Maybe ZoneCross
prove pass mark0 fixes i@(ZoneIdx i') j@(ZoneIdx j') bs = do
    fixM <- fixes ^? element i'
    fixN <- fixes ^? element j'
    let f = fixFromFix mark0
    let fs@[Fix{time = ti}, Fix{time = tj}] = [f i fixM, f j fixN]
    let zc = ZoneCross{crossingPair = fs, inZone = bs}
    if pass ti || pass tj then return zc else Nothing
{-# INLINABLE prove #-}

proveCrossing :: TimePass -> UTCTime -> [Kml.Fix] -> Crossing -> Maybe ZoneCross
proveCrossing pass mark0 fixes (Right (ZoneExit m n)) =
    prove pass mark0 fixes m n [True, False]

proveCrossing pass mark0 fixes (Left (ZoneEntry m n)) =
    prove pass mark0 fixes m n [False, True]
