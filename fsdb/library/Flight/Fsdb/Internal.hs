{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Flight.Fsdb.Internal (prs, sci, sciToInt, sciToFloat, sciToRational) where

import Text.XML.HXT.DOM.TypeDefs (XmlTree)
import Text.XML.HXT.Core
    ( ArrowXml
    , (&&&)
    , (>>>)
    , runX
    , withValidate
    , withWarnings
    , readString
    , no
    , hasName
    , getChildren
    , getAttrValue
    , deep
    , arr
    )

import Control.Monad
import Data.Bifunctor (bimap)
import Data.Functor.Identity (Identity)
import Flight.Comp (Comp(..), UtcOffset(..))

import Data.Void
import Data.Scientific
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

prs
  :: Parser a -- ^ Parser to run
  -> String -- ^ Input for the parser
  -> Either (ParseError Char Void) a -- ^ Result of parsing
prs p = parse p ""

sc :: Parser ()
sc = L.space (void spaceChar) empty empty

sci :: ParsecT Void String Identity Scientific
sci = L.signed sc L.scientific

sciToInt :: Scientific -> Integer
sciToInt = either round id . floatingOrInteger

sciToFloat :: Scientific -> Double
sciToFloat = either id fromIntegral . floatingOrInteger

sciToRational :: Scientific -> Rational
sciToRational = either toRational toRational . floatingOrInteger
