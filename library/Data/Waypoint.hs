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
    )
import qualified Text.ParserCombinators.Parsec as P

data Lat
    = LatN String String
    | LatS String String
    deriving Show

data Lng
    = LngW String String
    | LngE String String
    deriving Show

data IgcRecord
    = B String Lat Lng String String
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
    time <- count 6 (digit)
    lat' <- lat
    lng' <- lng
    _ <- oneOf "AV"
    altBaro <- count 5 (digit)
    altGps <- count 5 (digit)
    _ <- many (noneOf "\n")
    return $ B time lat' lng' altBaro altGps

ignore :: GenParser Char st IgcRecord
ignore = do
    return Ignore

eol :: GenParser Char st Char
eol = char '\n'

parse :: String -> Either ParseError [IgcRecord]
parse input = P.parse igcFile "(stdin)" input
