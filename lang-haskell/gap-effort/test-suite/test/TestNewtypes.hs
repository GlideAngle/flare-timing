module TestNewtypes where

-- NOTE: Avoid orphan instance warnings with these newtypes.

import Prelude hiding (max)
import qualified Prelude (max)
import Data.Refined (assumeProp, refined)
import Data.Ratio ((%))
import Data.List (sortBy)
import Test.SmallCheck.Series as SC
import Test.Tasty.QuickCheck as QC
import qualified Statistics.Sample as Stats (meanVariance)
import qualified Data.Vector as V (fromList)
import Data.UnitsOfMeasure (u, zero)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Test.QuickCheck.Instances ()

import Flight.Ratio (pattern (:%))
import "flight-gap-allot" Flight.Score
import "flight-gap-effort" Flight.Score

import Normal (Normal(..), NormalSum(..))
import Flight.Units()

-- | Difficulty fraction
newtype DfTest =
    DfTest
        ( FlownMax (Quantity Double [u| km |])
        , [PilotDistance (Quantity Double [u| km |])]
        )
    deriving Show

mkDfTest :: [Int] -> DfTest
mkDfTest xs =
    case toRational <$> sortBy (flip compare) (abs <$> xs) of
        [] ->
            DfTest
                ( FlownMax . MkQuantity $ 0
                , []
                )

        ys ->
            DfTest
                ( FlownMax . MkQuantity . fromRational $ maximum ys
                , PilotDistance . MkQuantity . fromRational <$> reverse ys
                )

instance Monad m => SC.Serial m DfTest where
    series = cons1 mkDfTest

instance QC.Arbitrary DfTest where
    arbitrary = do
        xs <- listOf $ choose (1, 1000000)
        return $ mkDfTest xs
