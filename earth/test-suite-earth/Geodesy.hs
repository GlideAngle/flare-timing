{-# LANGUAGE DuplicateRecordFields #-}

module Geodesy (DirectProblem(..), InverseProblem(..)) where

-- | The inputs for the direct or forward problem in geodesy.
data DirectProblem a α s =
    DirectProblem
        { x :: a -- ^ A point on the ellipsoid.
        , α :: α -- ^ An azimuth, an angle from North.
        , s :: s -- ^ A distance.
        }

-- | The inputs for the inverse or reverse problem in geodesy.
data InverseProblem a =
    InverseProblem
        { x :: a
        , y :: a
        }
