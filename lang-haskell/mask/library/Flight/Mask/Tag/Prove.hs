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

zoneCrossFixes :: UTCTime -> [Kml.Fix] -> ZoneIdx -> ZoneIdx -> Maybe (Fix, Fix)
zoneCrossFixes mark0 fixes i@(ZoneIdx i') j@(ZoneIdx j') = do
    fixM <- fixes ^? element i'
    fixN <- fixes ^? element j'
    let f = fixFromFix mark0
    return (f i fixM, f j fixN)

keep :: TimePass -> UTCTime -> [Kml.Fix] -> ZoneIdx -> ZoneIdx -> Bool
keep pass mark0 fixes i j = fromMaybe False $ do
    (Fix{time = ti}, Fix{time = tj}) <- zoneCrossFixes mark0 fixes i j
    return $ pass ti || pass tj

keepCrossing :: TimePass -> UTCTime -> [Kml.Fix] -> Crossing -> Bool
keepCrossing pass mark0 fixes (Left (ZoneEntry i j)) = keep pass mark0 fixes i j
keepCrossing pass mark0 fixes (Right (ZoneExit i j)) = keep pass mark0 fixes i j

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
prove pass mark0 fixes i j bs = do
    (f0@Fix{time = ti}, f1@Fix{time = tj}) <- zoneCrossFixes mark0 fixes i j
    let zc = ZoneCross{crossingPair = [f0, f1], inZone = bs}
    if pass ti || pass tj then return zc else Nothing
{-# INLINABLE prove #-}

proveCrossing :: TimePass -> UTCTime -> [Kml.Fix] -> Crossing -> Maybe ZoneCross
proveCrossing pass mark0 fixes (Right (ZoneExit m n)) =
    prove pass mark0 fixes m n [True, False]

proveCrossing pass mark0 fixes (Left (ZoneEntry m n)) =
    prove pass mark0 fixes m n [False, True]