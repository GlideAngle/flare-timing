module Test.Tasty.HUnit.Compare
    ( assertCompare
    , (@<?), (@?<)
    , (@<=?), (@?<=)
    , (@>?), (@?>)
    , (@>=?), (@?>=)
    ) where

import Prelude hiding (compare)
import Control.Monad
import Data.CallStack
import Test.Tasty.HUnit

assertCompare
  :: (Show a, HasCallStack)
  => String -- ^ The message prefix
  -> (a -> a -> Bool) -- ^ The comparison
  -> String -- ^ A symbol for the comparison
  -> a -- ^ The expected value
  -> a -- ^ The actual value
  -> Assertion
assertCompare preface compare cmpSymbol key actual =
    unless (actual `compare` key) (assertFailure msg)
    where
        msg =
            (if null preface then "" else preface ++ "\n") ++
            "expected: " ++ show actual ++ cmpSymbol ++ show key

infix 1 @<?, @?<, @<=?, @?<=, @>?, @?>, @>=?, @?>=

(@<?)
    :: (HasCallStack, Ord a, Show a)
    => a -- ^ The key value
    -> a -- ^ The actual value
    -> Assertion
key @<? actual = assertCompare "" (>) ">" key actual

(@?<)
    :: (HasCallStack, Ord a, Show a)
    => a -- ^ The actual value
    -> a -- ^ The key value
    -> Assertion
actual @?< key = assertCompare "" (<) "<" key actual

(@<=?)
    :: (HasCallStack, Ord a, Show a)
    => a -- ^ The key value
    -> a -- ^ The actual value
    -> Assertion
key @<=? actual = assertCompare "" (>=) ">=" key actual

(@?<=)
    :: (HasCallStack, Ord a, Show a)
    => a -- ^ The actual value
    -> a -- ^ The key value
    -> Assertion
actual @?<= key = assertCompare "" (<=) "<=" key actual

(@>?)
    :: (HasCallStack, Ord a, Show a)
    => a -- ^ The key value
    -> a -- ^ The actual value
    -> Assertion
key @>? actual = assertCompare "" (<=) "<=" key actual

(@?>)
    :: (HasCallStack, Ord a, Show a)
    => a -- ^ The actual value
    -> a -- ^ The key value
    -> Assertion
actual @?> key = assertCompare "" (>) ">" key actual

(@>=?)
    :: (HasCallStack, Ord a, Show a)
    => a -- ^ The key value
    -> a -- ^ The actual value
    -> Assertion
key @>=? actual = assertCompare "" (<) "<" key actual

(@?>=)
    :: (HasCallStack, Ord a, Show a)
    => a -- ^ The actual value
    -> a -- ^ The key value
    -> Assertion
actual @?>= key = assertCompare "" (>=) ">=" key actual
