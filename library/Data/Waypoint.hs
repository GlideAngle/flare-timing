module Data.Waypoint (parse) where

import Text.ParserCombinators.Parsec
    ( GenParser
    , ParseError
    , (<|>)
    , char
    , many
    , noneOf
    , eof
    )
import qualified Text.ParserCombinators.Parsec as P

data IgcRecord
    = B String
    | Ignore
    deriving Show

isB :: IgcRecord -> Bool
isB (B _) = True
isB _ = False

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
       
fix :: GenParser Char st IgcRecord
fix = do
    _ <- char 'B'
    rest <- many (noneOf "\n")
    return $ B rest

ignore :: GenParser Char st IgcRecord
ignore = do
    return Ignore

eol :: GenParser Char st Char
eol = char '\n'

parse :: String -> Either ParseError [IgcRecord]
parse input = P.parse igcFile "(stdin)" input
