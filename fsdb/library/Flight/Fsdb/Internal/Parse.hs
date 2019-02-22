{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Flight.Fsdb.Internal.Parse
    ( parseDegree
    , deg
    , ddd
    , dmm
    , dms
    ) where

import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec as P
import Data.Functor.Identity

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

float_
    ::
        ( m ~ ParsecT String String Identity
        , MonadParsec e s m
        , Token s ~ Char
        , RealFloat a
        )
    => m a
float_ = lexeme L.float

decimal_
    ::
        ( m ~ ParsecT String String Identity
        , MonadParsec e s m
        , Token s ~ Char
        , Integral a
        )
    => m a
decimal_ = lexeme L.decimal

-- |
-- >>> parseTest ddd "33.21354"
-- 33.21354
--
-- >>> parseTest ddd "-33.21354"
-- -33.21354
ddd :: ParsecT String String Identity Double
ddd = do
    degSign <- option id $ const negate <$> char '-'
    degs <- float_

    return $ degSign degs

-- |
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
    let degs = (fromIntegral dd) + mins / 60

    return $ degSign degs

-- |
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
    let degs = (fromIntegral dd) + (fromIntegral mm) / 60 + secs / 3600

    return $ degSign degs

-- |
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

parseDegree :: String -> Maybe Double
parseDegree = either (const Nothing) Just . P.parse deg "(stdin)"

-- $setup
-- >>> :set -XTypeSynonymInstances
-- >>> :set -XFlexibleInstances
-- >>> instance ShowErrorComponent String where showErrorComponent = id
