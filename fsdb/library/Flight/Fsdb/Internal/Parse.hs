module Flight.Fsdb.Internal.Parse
    ( prs
    , sci
    , sciToInt
    , sciToFloat
    , sciToRational
    ) where

import Control.Monad
import Data.Functor.Identity (Identity)

import Data.Void (Void)
import Data.Scientific (Scientific, floatingOrInteger)
import Text.Megaparsec (Parsec, ParseError, ParsecT, empty, parse)
import Text.Megaparsec.Char (spaceChar)
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
sciToInt =
    either round id
    . (floatingOrInteger :: Scientific -> Either Double Integer)

sciToFloat :: Scientific -> Double
sciToFloat =
    either id fromIntegral
    . (floatingOrInteger :: Scientific -> Either Double Integer)

sciToRational :: Scientific -> Rational
sciToRational =
    either toRational toRational
    . (floatingOrInteger :: Scientific -> Either Double Integer)
