{-|
Module: Flight.Clip
Copyright:
    © 2019 Phil de Joux
    © 2019 Block Scope Limited
License: MPL-2.0
Maintainer: Phil de Joux <phil.dejoux@blockscope.com>
Stability: experimental

Provides clipping of tracklogs.
-}
module Flight.Clip
    ( FlyCut(..)
    , FlyClipping(..)
    , FlyClipSection(..)
    , FlyingSection
    ) where

-- | A pair into the list of fixes marking those deemed logged while flying.
-- These could be indices, seconds offsets or UTC times.
type FlyingSection a = Maybe (a, a)

-- | The subset of the fixes that can be considered flown or scored.
data FlyCut a b =
    FlyCut
        { cut :: FlyingSection a
        , uncut :: b
        }
    deriving Show

class FlyClipping a b where
    clipToCut :: FlyCut a b -> FlyCut a b
    clipIndices :: FlyCut a b -> [Int]

class (FlyClipping a b) => FlyClipSection a b c where
    clipSection :: FlyCut a b -> FlyingSection c
