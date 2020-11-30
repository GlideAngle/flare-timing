{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.Fsdb.Internal.Parse
    ( parseDegree
    , deg
    , ddd
    , dmm
    , dms
    , parseUtcTime
    , parseHmsTime
    , parseUtcOffset
    ) where

import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (TimeOfDay)
import Data.Time.Format (parseTimeOrError, defaultTimeLocale)
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec as P
import Data.Functor.Identity
import Data.Scientific (Scientific, floatingOrInteger)

import Flight.Comp (UtcOffset(..))

-- |
-- >>> parseUtcTime "2012-01-05T12:00:00+11:00"
-- 2012-01-05 01:00:00 UTC
--
-- >>> parseUtcTime "2012-01-06T14:30:55+11:00"
-- 2012-01-06 03:30:55 UTC
--
-- >>> parseUtcTime "2012-01-06T17:50:09+11:00"
-- 2012-01-06 06:50:09 UTC
--
-- >>> parseUtcTime "2016-05-10T14:00:00-04:00"
-- 2016-05-10 18:00:00 UTC
--
-- >>> parseUtcTime "2016-05-10T17:46:43-04:00"
-- 2016-05-10 21:46:43 UTC
parseUtcTime :: String -> UTCTime
parseUtcTime =
    -- NOTE: %F is %Y-%m-%d, %T is %H:%M:%S and %z is -HHMM or -HH:MM
    parseTimeOrError False defaultTimeLocale "%FT%T%Z"

-- |
-- >>> parseHmsTime "12:00:00"
-- 12:00:00
--
-- >>> parseHmsTime "14:30:55"
-- 14:30:55
--
-- >>> parseHmsTime "17:50:09"
-- 17:50:09
--
-- >>> parseHmsTime "14:00:00"
-- 14:00:00
--
-- >>> parseHmsTime "17:46:43"
-- 17:46:43
parseHmsTime :: String -> TimeOfDay
parseHmsTime =
    parseTimeOrError False defaultTimeLocale "%T"

type Parser = Parsec String String

sc :: Parser ()
sc = L.space space1 lineComment blockComment
  where
    lineComment  = L.skipLineComment "//"
    blockComment = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

pNat :: ParsecT String String Identity Integer
pNat = decimal_

pFloat :: ParsecT String String Identity Double
pFloat = float_

float_ :: Parser Double
float_ = lexeme L.float

decimal_ :: Parser Integer
decimal_ = lexeme L.decimal

scientific_ :: Parser Scientific
scientific_ = lexeme L.scientific

-- | Parser for decimal degrees.
--
-- >>> parseTest ddd "33.21354"
-- 33.21354
--
-- >>> parseTest ddd "-33.21354"
-- -33.21354
ddd :: ParsecT String String Identity Double
ddd = do
    degSign <- option id $ const negate <$> char '-'
    degSign <$> float_

-- | Parser for degrees and decimal minutes.
--
-- >>> "33 " ++ show (0.21354 * 60)
-- "33 12.8124"
--
-- >>> parseTest dmm "33 12.8124"
-- 33.21354
--
-- >>> parseTest dmm "-33 12.8124"
-- -33.21354
dmm :: ParsecT String String Identity Double
dmm = do
    degSign <- option id $ const negate <$> char '-'
    dd <- pNat
    mins <- pFloat
    let degs = fromIntegral dd + mins / 60

    return $ degSign degs

-- | Parser for degrees minutes and decimal seconds.
--
-- >>> "33 12 " ++ show (0.8124 * 60)
-- "33 12 48.744"
--
-- >>> parseTest dms "33 12 48.744"
-- 33.21354
--
-- >>> parseTest dms "-33 12 48.744"
-- -33.21354
dms :: ParsecT String String Identity Double
dms = do
    degSign <- option id $ const negate <$> char '-'
    dd <- pNat
    mm <- pNat
    secs <- pFloat
    let degs = fromIntegral dd + fromIntegral mm / 60 + secs / 3600

    return $ degSign degs

-- | Parser that can handle decimal degrees, degress and decimal minutes or
-- degrees, minutes and decimal seconds.
--
-- >>> parseTest deg "33.21354"
-- 33.21354
--
-- >>> parseTest deg "-33.21354"
-- -33.21354
--
-- >>> parseTest deg "33 12.8124"
-- 33.21354
--
-- >>> parseTest deg "-33 12.8124"
-- -33.21354
--
-- >>> parseTest deg "33 12 48.744"
-- 33.21354
--
-- >>> parseTest deg "-33 12 48.744"
-- -33.21354
deg :: ParsecT String String Identity Double
deg = try ddd <|> try dmm <|> dms

-- | Parser for UTC offset.
--
-- >>> parseTest utcoffset "0"
-- Right 0
--
-- >>> parseTest utcoffset "+0"
-- Right 0
--
-- >>> parseTest utcoffset "-0"
-- Right 0
--
-- >>> parseTest utcoffset "1"
-- Right 1
--
-- >>> parseTest utcoffset "1.50"
-- Left 1.5
--
-- >>> parseTest utcoffset "-1.50"
-- Left (-1.5)
--
-- >>> parseTest utcoffset "+1"
-- Right 1
--
-- >>> parseTest utcoffset "-1"
-- Right (-1)
utcoffset :: ParsecT String String Identity (Either Double Int)
utcoffset = floatingOrInteger <$> L.signed (P.hidden C.space) scientific_

parseDegree :: String -> Maybe Double
parseDegree = either (const Nothing) Just . P.parse deg "(stdin)"

parseUtcOffset :: String -> Maybe UtcOffset
parseUtcOffset =
    either
        (const Nothing)
        (Just . UtcOffset . either (fromEnum . (* 60.0)) (* 60))
    . P.parse utcoffset "(stdin)"

-- $setup
-- >>> :set -XTypeSynonymInstances
-- >>> :set -XFlexibleInstances
-- >>> instance ShowErrorComponent String where showErrorComponent = id
