module Flight.Gap.Time.Leading (EssTime(..), LeadAllDown(..)) where

import GHC.Generics (Generic)
import "newtype" Control.Newtype (Newtype(..))
import Data.Via.Scientific
    ( DecimalPlaces(..)
    , deriveDecimalPlaces, deriveJsonViaSci, deriveCsvViaSci
    )

-- | Task time when the last pilot made the end of the speed section.
newtype EssTime = EssTime Rational
    deriving (Eq, Ord, Show, Generic)

instance Newtype EssTime Rational where
    pack = EssTime
    unpack (EssTime a) = a

deriveDecimalPlaces (DecimalPlaces 3) ''EssTime
deriveJsonViaSci ''EssTime
deriveCsvViaSci ''EssTime

-- | The time of last landing among all pilots, in seconds from first lead.
newtype LeadAllDown = LeadAllDown EssTime
