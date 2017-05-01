module Data.Waypoint (parse) where

import Text.ParserCombinators.Parsec
    ( GenParser
    , ParseError
    , (<|>)
    , char
    , many
    , oneOf
    , noneOf
    , count
    , digit
    , eof
    , optionMaybe
    )
import qualified Text.ParserCombinators.Parsec as P

data HMS = HMS String String String

data Lat
    = LatN String String
    | LatS String String

data Lng
    = LngW String String
    | LngE String String

data AltBaro = AltBaro String
data AltGps = AltGps String

showDegree :: String -> String
showDegree d = d ++ "°"

showMinute :: String -> String
showMinute (m0 : m1 : m) = [m0, m1] ++ "." ++ m ++ "'"
showMinute m = m 

showHMS :: HMS -> String
showHMS (HMS hh mm ss) = hh ++ ":" ++ mm ++ ":" ++ ss

showLat :: Lat -> String
showLat (LatN d m) = showDegree d ++ " " ++ showMinute m ++ " N"
showLat (LatS d m) = showDegree d ++ " " ++ showMinute m ++ " S"

showLng :: Lng -> String
showLng (LngW d m) = showDegree d ++ " " ++ showMinute m ++ " W"
showLng (LngE d m) = showDegree d ++ " " ++ showMinute m ++ " E"

ltrimZero :: String -> String
ltrimZero = dropWhile ((==) '0')

instance Show HMS where
    show = showHMS

instance Show Lat where
    show = showLat

instance Show Lng where
    show = showLng

instance Show AltBaro where
    show (AltBaro x) = ltrimZero x ++ "m"

instance Show AltGps where
    show (AltGps x) = ltrimZero x ++ "m"

data IgcRecord
    = B HMS Lat Lng AltBaro (Maybe AltGps)
    | Ignore
    deriving Show

isB :: IgcRecord -> Bool
isB (B _ _ _ _ _) = True
isB Ignore = False

igcFile :: GenParser Char st [IgcRecord]
igcFile = do
    result <- many line
    _ <- eof
    return $ filter isB result

line :: GenParser Char st IgcRecord
line = do
    result <- fix <|> ignore
    _ <- eol
    return result

hms :: GenParser Char st HMS
hms = do
    hh <- count 2 (digit)
    mm <- count 2 (digit)
    ss <- count 2 (digit)
    return $ HMS hh mm ss
       
lat :: GenParser Char st Lat
lat = do
    degs <- count 2 (digit)
    mins <- count 5 (digit)
    f <- const LatN <$> char 'N' <|> const LatS <$> char 'S'
    return $ f degs mins
       
lng :: GenParser Char st Lng
lng = do
    degs <- count 3 (digit)
    mins <- count 5 (digit)
    f <- const LngW <$> char 'W' <|> const LngE <$> char 'E'
    return $ f degs mins

altBaro :: GenParser Char st AltBaro
altBaro = do
    alt <- count 5 (digit)
    return $ AltBaro alt
       
altGps :: GenParser Char st AltGps
altGps = do
    alt <- count 5 (digit)
    return $ AltGps alt
       
{--
B: record type is a basic tracklog record
110135: <time> tracklog entry was recorded at 11:01:35 i.e. just after 11am
5206343N: <lat> i.e. 52 degrees 06.343 minutes North
00006198W: <long> i.e. 000 degrees 06.198 minutes West
A: <alt valid flag> confirming this record has a valid altitude value
00587: <altitude from pressure sensor>
00558: <altitude from GPS>
SEE: http://carrier.csi.cam.ac.uk/forsterlewis/soaring/igc_file_format/
--}
fix :: GenParser Char st IgcRecord
fix = do
    _ <- char 'B'
    hms' <- hms
    lat' <- lat
    lng' <- lng
    _ <- oneOf "AV"
    altBaro' <- altBaro
    altGps' <- optionMaybe altGps
    _ <- many (noneOf "\n")
    return $ B hms' lat' lng' altBaro' altGps'

ignore :: GenParser Char st IgcRecord
ignore = do
    return Ignore

eol :: GenParser Char st Char
eol = char '\n'

parse :: String -> Either ParseError [IgcRecord]
parse input = P.parse igcFile "(stdin)" input
