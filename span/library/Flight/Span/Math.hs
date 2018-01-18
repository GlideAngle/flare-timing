{-# LANGUAGE DeriveDataTypeable #-}

module Flight.Span.Math (Math(..)) where

import System.Console.CmdArgs.Implicit (Data, Typeable, Default(def))

data Math = Rational | Floating deriving (Eq, Data, Typeable, Show)

instance Default Math where
    def = Floating
