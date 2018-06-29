module Main (main) where

import Test.DocTest (doctest)

arguments :: [String]
arguments =
    [ "-isrc"
    , "library/Data/Via/UnitsOfMeasure.hs"
    , "-fobject-code"
    , "../units/library/Flight/Units.hs"
    , "../units/library/Flight/Units/Angle.hs"
    , "-fplugin"
    , "Data.UnitsOfMeasure.Plugin"
    ]

main :: IO ()
main = doctest arguments
