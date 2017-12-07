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
  :: (Eq a, Ord a, Enum a, Show a, HasCallStack)
  => String -- ^ The message prefix
  -> (a -> a -> Bool) -- ^ The comparison
  -> a      -- ^ The expected value
  -> a      -- ^ The actual value
  -> Assertion
assertCompare preface compare key actual =
    unless (actual `compare` key) (assertFailure msg)
    where
        cmp
            | key `compare` key =
              if succ key `compare` key then " >= " else
                if pred key `compare` key then " <= " else " == "
            | succ key `compare` key = " > "
            | pred key `compare` key = " < "
            | otherwise = " == "

        msg =
            (if null preface then "" else preface ++ "\n") ++
            "expected: " ++ show actual ++ cmp ++ show key

infix 1 @<?, @?<, @<=?, @?<=, @>?, @?>, @>=?, @?>=

(@<?)
    :: (HasCallStack, Eq a, Ord a, Enum a, Show a)
    => a -- ^ The key value
    -> a -- ^ The actual value
    -> Assertion
key @<? actual = assertCompare "" (>) key actual

(@?<)
    :: (HasCallStack, Eq a, Ord a, Enum a, Show a)
    => a -- ^ The actual value
    -> a -- ^ The key value
    -> Assertion
actual @?< key = assertCompare "" (<) key actual

(@<=?)
    :: (HasCallStack, Eq a, Ord a, Enum a, Show a)
    => a -- ^ The key value
    -> a -- ^ The actual value
    -> Assertion
key @<=? actual = assertCompare "" (>=) key actual

(@?<=)
    :: (HasCallStack, Eq a, Ord a, Enum a, Show a)
    => a -- ^ The actual value
    -> a -- ^ The key value
    -> Assertion
actual @?<= key = assertCompare "" (<=) key actual

(@>?)
    :: (HasCallStack, Eq a, Ord a, Enum a, Show a)
    => a -- ^ The key value
    -> a -- ^ The actual value
    -> Assertion
key @>? actual = assertCompare "" (<=) key actual

(@?>)
    :: (HasCallStack, Eq a, Ord a, Enum a, Show a)
    => a -- ^ The actual value
    -> a -- ^ The key value
    -> Assertion
actual @?> key = assertCompare "" (>) key actual

(@>=?)
    :: (HasCallStack, Eq a, Ord a, Enum a, Show a)
    => a -- ^ The key value
    -> a -- ^ The actual value
    -> Assertion
key @>=? actual = assertCompare "" (<) key actual

(@?>=)
    :: (HasCallStack, Eq a, Ord a, Enum a, Show a)
    => a -- ^ The actual value
    -> a -- ^ The key value
    -> Assertion
actual @?>= key = assertCompare "" (>=) key actual
