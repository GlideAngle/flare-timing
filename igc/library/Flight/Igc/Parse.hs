module Flight.Igc.Parse
    ( parse
    , parseFromFile
    ) where

import Prelude hiding (readFile)
import Data.ByteString.UTF8 (toString)
import Data.ByteString (readFile)
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char
import qualified Text.Megaparsec as P (parse)
import Data.Void
import Data.Functor.Identity
import Flight.Igc.Record

igcFile :: ParsecT Void String Identity [IgcRecord]
igcFile = do
    hfdte <- try p1 <|> try p2
    lines' <- manyTill anySingle (char 'B') *> many line
    _ <- eof
    return $ hfdte : lines'
    where
        p1 = manyTill anySingle (lookAhead (string "HFDTEDATE:")) *> headerLine dateHFDTEDATE
        p2 = manyTill anySingle (lookAhead (string "HFDTE")) *> headerLine dateHFDTE
        headerLine date = do
            line' <- date
            _ <- eol
            return line'

line :: ParsecT Void String Identity IgcRecord
line = do
    line' <- fix <|> ignore
    _ <- eol
    return line'

-- |
-- >>> parseTest timeHHMMSS "0200223"
-- 02:00:22
timeHHMMSS :: ParsecT Void String Identity HMS
timeHHMMSS = do
    hh <- Hour . read <$> count 2 digitChar
    mm <- Minute <$> count 2 digitChar
    ss <- Second . read <$> count 2 digitChar
    return $ HMS hh mm ss

-- |
-- >>> parseTest lat "3321354S"
-- 33° 21.354' S
lat :: ParsecT Void String Identity Lat
lat = do
    degs <- Degree . read <$> count 2 digitChar
    mins <- Minute <$> count 5 digitChar
    f <- const LatN <$> char 'N' <|> const LatS <$> char 'S'
    return $ f degs mins

-- |
-- >>> parseTest lng "14756057E"
-- 147° 56.057' E
lng :: ParsecT Void String Identity Lng
lng = do
    degs <- Degree . read <$> count 3 digitChar
    mins <- Minute <$> count 5 digitChar
    f <- const LngW <$> char 'W' <|> const LngE <$> char 'E'
    return $ f degs mins

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
altBaro :: ParsecT Void String Identity AltBaro
altBaro =
    AltBaro . Altitude
    <$>
        ( count 5 digitChar
        <|> (char '-' >> (("-" ++) <$> count 4 digitChar))
        )

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
altGps :: ParsecT Void String Identity AltGps
altGps =
    AltGps . Altitude
    <$>
        ( count 5 digitChar
        <|> (char '-' >> (("-" ++) <$> count 4 digitChar))
        )

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
alt :: ParsecT Void String Identity (AltBaro, Maybe AltGps)
alt = do
    _ <- oneOf ("AV" :: String)
    altBaro' <- altBaro
    altGps' <- optional altGps
    return (altBaro', altGps')

-- |
-- >>> parseTest fix "B0200223321354S14756057EA0024400241000\n"
-- 02:00:22 33° 21.354' S 147° 56.057' E 244m (Just 241m)
--
-- >>> parseTest fix "B1700582832124N08150806WA0000000042\n"
-- 17:00:58 28° 32.124' N 081° 50.806' W 0m (Just 42m)
--
-- >>> parseTest fix "B1701282832124N08150806WA-000100043\n"
-- 17:01:28 28° 32.124' N 081° 50.806' W -1m (Just 43m)
fix :: ParsecT Void String Identity IgcRecord
fix = do
    _ <- char 'B'
    hms' <- timeHHMMSS
    lat' <- lat
    lng' <- lng
    (altBaro', altGps') <- alt
    _ <- many (noneOf ("\n" :: String))
    return $ B hms' (lat', lng',  altBaro',  altGps')

-- |
-- >>> parseTest dateHFDTEDATE "HFDTEDATE:030118,01"
-- 2018-01-03, 01
dateHFDTEDATE :: ParsecT Void String Identity IgcRecord
dateHFDTEDATE = do
    _ <- string "HFDTEDATE:"

    dd <- Day . read <$> count 2 digitChar
    mm <- Month . read <$> count 2 digitChar
    yy <- Year . read <$> count 2 digitChar
    let ymd = YMD {year = yy, month = mm, day = dd}

    _ <- string ","
    nn <- Nth <$> count 2 digitChar

    return $ HFDTEDATE {ymd = ymd, nth = nn}

-- |
-- >>> parseTest dateHFDTE "HFDTE0301181"
-- 2018-01-03
dateHFDTE :: ParsecT Void String Identity IgcRecord
dateHFDTE = do
    _ <- string "HFDTE"

    dd <- Day . read <$> count 2 digitChar
    mm <- Month . read <$> count 2 digitChar
    yy <- Year . read <$> count 2 digitChar
    let ymd = YMD {year = yy, month = mm, day = dd}

    return $ HFDTE ymd

ignore :: ParsecT Void String Identity IgcRecord
ignore = do
    _ <- many (noneOf ("\n" :: String))
    return Ignore

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
-- 03:13:00 33° 21.380' S 147° 55.984' E 283m (Just 237m)
-- 03:13:01 33° 21.385' S 147° 55.977' E 290m (Just 241m)
-- ... plus 13175 other B records
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
-- 02:37:56 27° 09.269' S 151° 14.965' E 0m (Just 1004m)
-- 02:37:58 27° 09.274' S 151° 14.971' E 0m (Just 1010m)
-- ... plus 9733 other B records
-- <BLANKLINE>
--
-- >>> parse igcGordon
-- Right 2018-01-02
-- 00:44:29 33° 21.373' S 147° 56.064' E 285m (Just 0m)
-- 00:44:30 33° 21.369' S 147° 56.061' E 285m (Just 0m)
-- ... plus 30026 other B records
-- <BLANKLINE>
parse
   :: String -- ^ A string to parse
   -> Either (ParseErrorBundle String Void) [IgcRecord]
parse = P.parse igcFile "(stdin)"

parseFromFile
    :: FilePath -- ^ An IGC file to parse.
    -> IO (Either (ParseErrorBundle String Void) [IgcRecord])
parseFromFile fname =
    runParser igcFile fname . toString <$> readFile fname

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
--
-- >>> igcScott = $(embedStr (System.IO.readFile fileScott))
-- >>> igcGordon = $(embedStr (System.IO.readFile fileGordon))
