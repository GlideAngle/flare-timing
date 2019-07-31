module Flight.Mask.Tag.Prove
    ( TimePass
    , prove
    , proveCrossing
    ) where

import Prelude hiding (span)
import Data.Time.Clock (UTCTime)
import Control.Lens ((^?), element)

import Flight.Units ()
import qualified Flight.Kml as Kml (Fix)
import Flight.Track.Cross (Fix(..), ZoneCross(..))
import Flight.Track.Time (ZoneIdx(..))

import Flight.Mask.Internal.Race ()
import Flight.Mask.Internal.Zone
    ( ZoneEntry(..)
    , ZoneExit(..)
    , Crossing
    , fixFromFix
    )

type TimePass = UTCTime -> Bool

-- | Prove from the fixes and mark that the crossing exits.
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
    if pass ti && pass tj then return zc else Nothing

proveCrossing :: TimePass -> UTCTime -> [Kml.Fix] -> Crossing -> Maybe ZoneCross
proveCrossing pass mark0 fixes (Right (ZoneExit m n)) =
    prove pass mark0 fixes m n [True, False]

proveCrossing pass mark0 fixes (Left (ZoneEntry m n)) =
    prove pass mark0 fixes m n [False, True]
