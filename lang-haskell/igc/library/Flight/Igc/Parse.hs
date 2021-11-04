module Flight.Igc.Parse
    ( parse
    , parseFromFile
    , parseTest
    ) where

import Prelude hiding (readFile)

import Data.ByteString (ByteString, readFile)
import Data.ByteString.Char8 (pack)
import qualified Data.Attoparsec.ByteString as P (parseOnly)
import Data.Attoparsec.ByteString.Char8
    ( char, string, digit, anyChar
    , option, satisfy, many', manyTill, count
    , endOfLine, endOfInput
    )
import Data.Attoparsec.ByteString (Parser)
import Data.Attoparsec.Combinator (lookAhead, try)
import Data.Char (digitToInt)
import Control.Applicative ((<|>))
    --, count, string, many'
import Flight.Igc.Record
    ( IgcRecord(..), YMD(..), HMS(..), AltGps(..), AltBaro(..), Lat(..), Lng(..)
    , Nth(..), Second(..), Hour(..), Day(..), Month(..), Year(..), Altitude(..)
    , Degree(..), MinuteOfAngle(..), MinuteOfTime(..)
    )

digitInt :: Parser Int
digitInt = digitToInt <$> digit

-- SEE: https://stackoverflow.com/questions/30704459/how-can-i-parse-fixed-length-non-delimited-integers-with-attoparsec
digitInt2 :: Parser Int
digitInt2 =
    (\x y -> 10 * x + y)
    <$> digitInt
    <*> digitInt

digitInt3 :: Parser Int
digitInt3 =
    (\x y z -> 100 * x + 10 * y + z)
    <$> digitInt
    <*> digitInt
    <*> digitInt

digitInt5 :: Parser Int
digitInt5 =
    (\x y z a b -> 10000 * x + 1000 * y + 100 * z + 10 * a + b)
    <$> digitInt
    <*> digitInt
    <*> digitInt
    <*> digitInt
    <*> digitInt

digitInt4Neg :: Parser Int
digitInt4Neg =
    (\_ y z a b -> -(1000 * y + 100 * z + 10 * a + b))
    <$> char '-'
    <*> digitInt
    <*> digitInt
    <*> digitInt
    <*> digitInt

igcFile :: Parser [IgcRecord]
igcFile = do
    hfdte <- try p1 <|> try p2
    lines' <- lookAhead (manyTill anyChar (char 'B')) *> many' line
    _ <- endOfInput
    return $ hfdte : lines'
    where
        p1 = manyTill anyChar (lookAhead (string "HFDTEDATE:")) *> headerLine dateHFDTEDATE
        p2 = manyTill anyChar (lookAhead (string "HFDTE")) *> headerLine dateHFDTE
        headerLine date = date <* endOfLine

line :: Parser IgcRecord
line =
    fix <* endOfLine
    -- WARNING: The security record in the IGC file is not always followed by
    -- an end of line before the end of file.
    <|> security <* (endOfLine <|> endOfInput)
    <|> ignore <* endOfLine
{-# INLINE line #-}

-- |
-- >>> parseTest timeHHMMSS "0200223"
-- 02:00:22
timeHHMMSS :: Parser HMS
timeHHMMSS = do
    hh <- Hour <$> digitInt2
    mm <- MinuteOfTime <$> digitInt2
    ss <- Second <$> digitInt2
    return $ HMS hh mm ss
{-# INLINE timeHHMMSS #-}

-- |
-- >>> parseTest lat "3321354S"
-- 33° 21.354' S
lat :: Parser Lat
lat = do
    degs <- Degree <$> digitInt2
    mins <- MinuteOfAngle <$> digitInt5
    f <- const LatN <$> char 'N' <|> const LatS <$> char 'S'
    return $ f degs mins
{-# INLINE lat #-}

-- |
-- >>> parseTest lng "14756057E"
-- 147° 56.057' E
lng :: Parser Lng
lng = do
    degs <- Degree <$> digitInt3
    mins <- MinuteOfAngle <$> digitInt5
    f <- const LngW <$> char 'W' <|> const LngE <$> char 'E'
    return $ f degs mins
{-# INLINE lng #-}

-- |
-- >>> parseTest altBaro "00244"
-- 244m
--
-- >>> parseTest altBaro "00000"
-- 0m
--
-- >>> parseTest altBaro "-0000"
-- 0m
--
-- >>> parseTest altBaro "-0001"
-- -1m
altBaro :: Parser AltBaro
altBaro = AltBaro . Altitude <$> (digitInt5 <|> digitInt4Neg)
{-# INLINE altBaro #-}

-- |
-- >>> parseTest altGps "00244"
-- 244m
--
-- >>> parseTest altGps "00000"
-- 0m
--
-- >>> parseTest altGps "-0000"
-- 0m
--
-- >>> parseTest altGps "-0001"
-- -1m
altGps :: Parser AltGps
altGps = AltGps . Altitude <$> (digitInt5 <|> digitInt4Neg)
{-# INLINE altGps #-}

-- |
-- >>> parseTest alt "A0024400241000"
-- (244m,Just 241m)
--
-- >>> parseTest alt "V0024400241000"
-- (244m,Just 241m)
--
-- >>> parseTest alt "A-000100043"
-- (-1m,Just 43m)
--
-- >>> parseTest alt "A-0001-0043"
-- (-1m,Just -43m)
alt :: Parser (AltBaro, Maybe AltGps)
alt = do
    _ <- altLeadingChar
    altBaro' <- altBaro
    altGps' <- option Nothing (Just <$> altGps)
    return (altBaro', altGps')
{-# INLINE alt #-}

altLeadingChar :: Parser Char
altLeadingChar = satisfy (\x -> x == 'A' || x == 'V')
{-# INLINE altLeadingChar #-}

-- |
-- >>> parseTest fix "B0200223321354S14756057EA0024400241000\n"
-- 02:00:22 33° 21.354' S 147° 56.057' E 244m (Just 241m)
--
-- >>> parseTest fix "B1700582832124N08150806WA0000000042\n"
-- 17:00:58 28° 32.124' N 081° 50.806' W 0m (Just 42m)
--
-- >>> parseTest fix "B1701282832124N08150806WA-000100043\n"
-- 17:01:28 28° 32.124' N 081° 50.806' W -1m (Just 43m)
fix :: Parser IgcRecord
fix = do
    _ <- char 'B'
    hms' <- timeHHMMSS
    lat' <- lat
    lng' <- lng
    (altBaro', altGps') <- alt
    _ <- many' notNewLine
    return $ B hms' (lat', lng',  altBaro',  altGps')
{-# INLINE fix #-}

security :: Parser IgcRecord
security = do
    _ <- char 'G'
    _ <- many' notNewLine
    return G
{-# INLINE security #-}

parseYMD :: Parser YMD
parseYMD = do
    dd <- Day <$> digitInt2
    mm <- Month <$> digitInt2
    yy <- Year <$> digitInt2
    return $ YMD {year = yy, month = mm, day = dd}

-- |
-- >>> parseTest dateHFDTEDATE "HFDTEDATE:030118,01"
-- 2018-01-03, 01
dateHFDTEDATE :: Parser IgcRecord
dateHFDTEDATE = do
    _ <- string "HFDTEDATE:"
    ymd <- parseYMD

    _ <- string ","
    nn <- Nth <$> count 2 digit

    return $ HFDTEDATE {ymd = ymd, nth = nn}
{-# INLINE dateHFDTEDATE #-}

-- |
-- >>> parseTest dateHFDTE "HFDTE0301181"
-- 2018-01-03
dateHFDTE :: Parser IgcRecord
dateHFDTE = do
    _ <- string "HFDTE"
    HFDTE <$> parseYMD
{-# INLINE dateHFDTE #-}

ignore :: Parser IgcRecord
ignore = do
    _ <- many' notNewLine
    return Ignore
{-# INLINE ignore #-}

notNewLine :: Parser Char
notNewLine = satisfy (\x -> x /= '\r' && x /= '\n')
{-# INLINE notNewLine #-}

-- |
-- >>> line 1 igcHFDTE
-- "HFDTE030118\n"
-- >>> line 9 igcHFDTE
-- "B0405473321383S14756040EA0024800227\n"
-- >>> line 3527 igcHFDTE
-- "B0613553236449S14817636EA0036500338\n"
-- >>> line 3529 igcHFDTE
-- "LXGD Downloaded 2018-01-03  21:30:14\n"
--
-- >>> parse igcHFDTE
-- Right 2018-01-03
-- G
-- 04:05:47 33° 21.383' S 147° 56.040' E 248m (Just 227m)
-- 04:05:48 33° 21.388' S 147° 56.036' E 249m (Just 227m)
-- ... plus 3530 other B records
-- <BLANKLINE>
--
-- >>> line 1 igcHFDTEDATE
-- "HFDTEDATE:030118,01\n"
-- >>> line 9 igcHFDTEDATE
-- "B0312593321375S14755988EA0027700235\n"
-- >>> line 13179 igcHFDTEDATE
-- "B0657503228681S14842328EA0034500302\n"
-- >>> line 13181 igcHFDTEDATE
-- "LXGD Downloaded 2018-01-03  12:12:46\n"
--
-- >>> parse igcHFDTEDATE
-- Right 2018-01-03, 01
-- G
-- 03:12:59 33° 21.375' S 147° 55.988' E 277m (Just 235m)
-- 03:13:00 33° 21.380' S 147° 55.984' E 283m (Just 237m)
-- ... plus 13182 other B records
-- <BLANKLINE>
--
-- >>> line 1 igcScott
-- "HFDTE080417\n"
--
-- >>> line 1 igcGordon
-- "HFDTE020118\n"
--
-- >>> parse igcScott
-- Right 2017-04-08
-- G
-- 02:37:56 27° 09.269' S 151° 14.965' E 0m (Just 1004m)
-- 02:37:58 27° 09.274' S 151° 14.971' E 0m (Just 1010m)
-- ... plus 9733 other B records
-- <BLANKLINE>
--
-- >>> parse igcGordon
-- Right 2018-01-02
-- G
-- 00:44:29 33° 21.373' S 147° 56.064' E 285m (Just 0m)
-- 00:44:30 33° 21.369' S 147° 56.061' E 285m (Just 0m)
-- ... plus 30033 other B records
-- <BLANKLINE>
--
-- >>> parse igcPetros
-- Right 2019-07-11
-- G
-- 12:04:01 43° 47.488' N 016° 29.815' E 0m (Just 1192m)
-- 12:04:03 43° 47.488' N 016° 29.815' E 0m (Just 1192m)
-- ... plus 1757 other B records
-- <BLANKLINE>
parse
   :: ByteString -- ^ A string to parse
   -> Either String [IgcRecord]
parse = P.parseOnly igcFile

parseFromFile
    :: FilePath -- ^ An IGC file to parse.
    -> IO (Either String [IgcRecord])
parseFromFile fname = do
    c <- readFile fname
    return $ P.parseOnly igcFile c

parseTest :: Show a => Parser a -> String -> IO ()
parseTest p s = either print print . P.parseOnly p $ pack s

-- NOTE: The attoparsec parseTest has different output than the one I'd used
-- with megaparsec.
-- > parseTest altBaro "00244"'
-- expected: 244m
--  but got: Done "" 244m

-- $setup
-- >>> :set -XTemplateHaskell
-- >>> import Language.Haskell.TH
-- >>> import Language.Haskell.TH.Syntax (lift)
-- >>> import Flight.Igc.Parse (parse)
-- :{
-- embedStr :: IO String -> ExpQ
-- embedStr readStr = lift =<< runIO readStr
-- :}
--
-- >>> line n = unlines . take 1 . drop n . lines
--
-- >>> fileHFDTEDATE  = "./test-suite-doctest/Sasha-Serebrennikova.20180103-121306.30169.72.igc"
-- >>> fileHFDTE  = "./test-suite-doctest/Brad-Porter.20180104-095852.36822.34.igc"
--
-- >>> igcHFDTEDATE = $(embedStr (System.IO.readFile fileHFDTEDATE))
-- >>> igcHFDTE = $(embedStr (System.IO.readFile fileHFDTE))
--
-- >>> fileScott = "./test-suite-doctest/Scott-Barrett.20170409-071936.7601.19.igc"
-- >>> fileGordon = "./test-suite-doctest/Gordon_Rigg.20180103-111847.6433.8.igc"
-- >>> filePetros = "./test-suite-doctest/Petros_Miskos.20190711-175327.43547.18.igc"
--
-- >>> igcScott = $(embedStr (System.IO.readFile fileScott))
-- >>> igcGordon = $(embedStr (System.IO.readFile fileGordon))
-- >>> igcPetros = $(embedStr (System.IO.readFile filePetros))
