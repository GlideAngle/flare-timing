module Flight.Kml.Internal
    (
    -- * Internal Usage
    -- $internal-use

    -- ** Display of a fix
      showLatLngAlt
    , showLngLatAlt
    , showTimeAlt

    -- ** Length and range
    , fixesLength
    , fixesSecondsRange
    , fixesUTCTimeRange

    -- ** Display of fixes
    , showFixesLength
    , showFixesSecondsRange
    , showFixesUTCTimeRange

    -- * Parsing
    , formatFloat
    , roundTripLatLngAlt
    , parseTimeOffsets
    , parseBaroMarks
    , parseLngLatAlt
    , parseUtcTime

    ) where

import Data.List.Split (splitOn)
import Numeric (showFFloat)
import Data.Time.Clock (UTCTime, addUTCTime)
import Data.Time.Format (parseTimeM, defaultTimeLocale)
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec as P
import Data.Functor.Identity
import Data.Void
import Flight.Types
    ( Latitude(..), Longitude(..), Altitude(..), Seconds(..)
    , LLA(..), Fix(..)
    , MarkedFixes(..)
    , FixMark(..)
    )

type Parser = Parsec String String

sc :: Parser ()
sc = L.space space1 lineComment blockComment
  where
    lineComment  = L.skipLineComment "//"
    blockComment = L.skipBlockComment "/*" "*/"
{-# INLINE sc #-}

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc
{-# INLINE lexeme #-}

pFloat :: ParsecT String String Identity Rational
pFloat = (toRational :: Double -> Rational) <$> float_
{-# INLINE pFloat #-}

pNat :: ParsecT String String Identity Integer
pNat = decimal_
{-# INLINE pNat #-}

float_ :: (m ~ ParsecT String String Identity, RealFloat a) => m a
float_ = lexeme L.float
{-# INLINE float_ #-}

decimal_ :: (m ~ ParsecT String String Identity, Integral a) => m a
decimal_ = lexeme L.decimal
{-# INLINE decimal_ #-}

pNats :: Parsec String String [Integer]
pNats = do
    _ <- space
    xs <- pNat `sepBy` space
    _ <- eof
    return xs
{-# INLINE pNats #-}

-- | Parses UTC time in the format yyyy-MM-ddThh:mm:ssZ.
--
-- >>> parseUtcTime "2012-01-14T08:22:21Z"
-- Just 2012-01-14 08:22:21 UTC
parseUtcTime :: String -> Maybe UTCTime
parseUtcTime s =
    case P.parse pUtcTimeZ "(stdin)" s of
        Left _ -> Nothing
        Right t -> Just t
{-# INLINE parseUtcTime #-}

pUtcTimeZ :: ParsecT Void String Identity UTCTime
pUtcTimeZ = do
    ymd <- many $ satisfy ('T' /=)
    _ <- char 'T'
    hrs <- count 2 digitChar
    _ <- char ':'
    mins <- count 2 digitChar
    _ <- char ':'
    secs <- count 2 digitChar
    zulu <- option "Z" (string "Z")

    let s = mconcat [ymd, "T", hrs, ":", mins, ":", secs, zulu]
    let t = parseTimeM False defaultTimeLocale "%FT%TZ" s

    case t of
        Nothing -> empty
        Just t' -> return t'
{-# INLINE pUtcTimeZ #-}

pFix :: ParsecT String String Identity (Rational, Rational, Integer)
pFix = do
    -- NOTE: KML coordinates have a space between tuples.
    -- lon,lat[,alt]
    -- SEE: https://developers.google.com/kml/documentation/kmlreference#linestring
    lngSign <- option id $ const negate <$> char '-'
    lng <- pFloat <?> "No longitude"
    _ <- char ','
    latSign <- option id $ const negate <$> char '-'
    lat <- pFloat <?> "No latitude"
    _ <- char ','
    altSign <- option id $ const negate <$> char '-'
    alt <- pNat <?> "No altitude"
    return (latSign lat, lngSign lng, altSign alt)
{-# INLINE pFix #-}

pFixes :: ParsecT String String Identity [ (Rational, Rational, Integer) ]
pFixes = do
    _ <- space
    xs <- pFix `sepBy` space <?> "No fixes"
    _ <- eof
    return xs
{-# INLINE pFixes #-}

-- | Parse the list of time offsets.
--
-- >>> parseTimeOffsets "0 2 4 6 8 10 12 14 16 18 20 22 24 26 28 30"
-- [0s,2s,4s,6s,8s,10s,12s,14s,16s,18s,20s,22s,24s,26s,28s,30s]
parseTimeOffsets :: String -> [Seconds]
parseTimeOffsets s =
    case P.parse pNats "(stdin)" s of
        Left _ -> []
        Right xs -> Seconds <$> xs
{-# INLINE parseTimeOffsets #-}

-- | Parse the list of barometric pressure altitudes.
-- 
-- >>> parseBaroMarks "239 240 240 239 239 239 239 239 239 240 239 240 239 239 240"
-- [239m,240m,240m,239m,239m,239m,239m,239m,239m,240m,239m,240m,239m,239m,240m]
parseBaroMarks :: String -> [Altitude]
parseBaroMarks s =
    case P.parse pNats "(stdin)" s of
         Left _ -> []
         Right xs -> Altitude <$> xs
{-# INLINE parseBaroMarks #-}

-- | Parse comma-separated triples of lng,lat,alt, each triple separated by
-- spaces.
--
-- >>> parseLngLatAlt "147.932050,-33.361600,237 147.932050,-33.361600,238"
-- [LLA {llaLat = -33.36160000°, llaLng = 147.93205000°, llaAltGps = 237m},LLA {llaLat = -33.36160000°, llaLng = 147.93205000°, llaAltGps = 238m}]
parseLngLatAlt :: String -> [LLA]
parseLngLatAlt s =
    case P.parse pFixes "(stdin)" s of
         Left _ -> []
         Right xs ->
             (\(lat, lng, alt) ->
                 LLA
                     (Latitude lat)
                     (Longitude lng)
                     (Altitude alt)) <$> xs
{-# INLINE parseLngLatAlt #-}

-- | Avoids __@"0."@__ because ...
-- 
-- @
-- > (read "0." :: Double)
-- Exception: Prelude.read: no parse
-- > (read "0.0" :: Double)
-- 0.0
-- @
--
-- >>> formatFloat "112.2334455"
-- "112.233446"
-- >>> formatFloat "0"
-- "0.000000"
-- >>> formatFloat "0."
-- "0.000000"
-- >>> formatFloat "0.0"
-- "0.000000"
formatFloat :: String -> String
formatFloat s =
    case splitOn "." s of
         [ a, "" ] -> showFFloat (Just 6) (read a :: Double) ""
         _ -> showFFloat (Just 6) (read s :: Double) ""

-- | Shows relative time offset in seconds and altitude in metres.
--
-- >>> import Flight.Kml (mkPosition)
-- >>> let lla = mkPosition (Latitude (-33.65073300), Longitude 147.56036700, Altitude 214)
-- >>> showTimeAlt $ Fix (Seconds 0) lla Nothing
-- "(0s,214m)"
showTimeAlt :: Fix -> String
showTimeAlt Fix{fixMark, fix} =
    "(" ++ show s ++ "s," ++ show a ++ "m)"
    where
        Seconds s = fixMark
        LLA{llaAltGps} = fix
        Altitude a = llaAltGps

-- | Shows lat,lng,alt.
--
-- >>> showLatLngAlt (Latitude (-33.65073300), Longitude 147.56036700, Altitude 214)
-- "-33.650733,147.560367,214"
showLatLngAlt :: (Latitude, Longitude, Altitude) -> String
showLatLngAlt (Latitude lat, Longitude lng, Altitude alt) =
    mconcat [ formatFloat $ show (fromRational lat :: Double)
            , ","
            , formatFloat $ show (fromRational lng :: Double)
            , ","
            , show alt
            ]

-- | Shows lng,lat,alt.
--
-- >>> showLngLatAlt (Latitude (-33.65073300), Longitude 147.56036700, Altitude 214)
-- "147.560367,-33.650733,214"
showLngLatAlt :: (Latitude, Longitude, Altitude) -> String
showLngLatAlt (Latitude lat, Longitude lng, Altitude alt) =
    mconcat [ formatFloat $ show (fromRational lng :: Double)
            , ","
            , formatFloat $ show (fromRational lat :: Double)
            , ","
            , show alt
            ]

-- | Round trip from rational to double and back to rational.
-- 
-- >>> roundTripLatLngAlt (Latitude (-33.65073300), Longitude 147.56036700, Altitude 214)
-- (-33.650733,147.560367,214m)
roundTripLatLngAlt :: (Latitude, Longitude, Altitude)
                   -> (Double, Double, Altitude)
roundTripLatLngAlt (Latitude lat, Longitude lng, alt) =
    let lat' = read $ formatFloat $ show (fromRational lat :: Double)
        lng' = read $ formatFloat $ show (fromRational lng :: Double)
    in (lat', lng', alt)

-- | The number of fixes in the track log.  There is a <#range fixesLength>
-- example in the usage section.
fixesLength :: MarkedFixes -> Int
fixesLength MarkedFixes{fixes} =
    length fixes

-- | In the given list of fixes, the seconds offset of the first and last
-- elements.  There is a <#range fixesSecondsRange> example in the usage
-- section.
fixesSecondsRange :: MarkedFixes -> Maybe (Seconds, Seconds)
fixesSecondsRange MarkedFixes{fixes} =
    case (fixes, reverse fixes) of
        ([], _) -> Nothing
        (_, []) -> Nothing
        (x : _, y : _) -> Just (mark x, mark y)

-- | In the given list of fixes, the UTC time of the first and last elements.
-- There is a <#range fixesUTCTimeRange> example in the usage section.
fixesUTCTimeRange :: MarkedFixes -> Maybe (UTCTime, UTCTime)
fixesUTCTimeRange mf@MarkedFixes{mark0} =
    rangeUTCTime mark0 <$> fixesSecondsRange mf

-- | Shows the number of elements in the list of fixes, in the tracklog.  There
-- is a <#showfixes showFixesLength> example in the usage section.
showFixesLength :: MarkedFixes -> String
showFixesLength = show . fixesLength

-- | Shows the relative time range of the tracklog.  There is a
-- <#showfixes showFixesSecondsRange> example in the usage section.
showFixesSecondsRange :: MarkedFixes -> String
showFixesSecondsRange mf =
    maybe "[]" show (fixesSecondsRange mf)

-- | Shows the absolute time range of the tracklog.  There is a
-- <#showfixes showFixesUTCTimeRange> example in the usage section.
showFixesUTCTimeRange :: MarkedFixes -> String
showFixesUTCTimeRange mf@MarkedFixes{mark0} =
    maybe "" (show . rangeUTCTime mark0) (fixesSecondsRange mf)

-- | By providing the UTC time of the first fix, convert a relative time range
-- of offset seconds into a time absolute time range of UTC times.
rangeUTCTime :: UTCTime -> (Seconds, Seconds) -> (UTCTime, UTCTime)
rangeUTCTime mark0 (Seconds s0, Seconds s1) =
    let f secs = fromInteger secs `addUTCTime` mark0 in (f s0, f s1)

-- $setup
-- >>> :set -XTemplateHaskell
-- >>> :set -XNamedFieldPuns
-- >>> import Language.Haskell.TH
-- >>> import Language.Haskell.TH.Syntax (lift)
-- >>> import Flight.Kml
-- >>> import Flight.Kml.Internal (showLatLngAlt, showLngLatAlt, showTimeAlt)
-- :{
-- embedStr :: IO String -> ExpQ
-- embedStr readStr = lift =<< runIO readStr
-- :}
-- 
-- >>> kml = $(embedStr (readFile "./test-suite-doctest/Phil-de-Joux.20120114-082221.21437.40.kml"))
-- 

-- $internal-use
-- Working with the <Flight-Kml.html#kml KML tracklog dump> from the tracklog file "__@Phil-de-Joux.20120114-082221.21437.40.kml@__".
--
-- >>> Right mf@(MarkedFixes{mark0, fixes}) <- parse kml
-- >>> mark0
-- 2012-01-14 02:12:55 UTC
-- >>> length fixes
-- 6547
-- >>> head fixes
-- Fix {fixMark = 0s, fix = LLA {llaLat = -33.36160000°, llaLng = 147.93205000°, llaAltGps = 237m}, fixAltBaro = Just 239m}
-- >>> last fixes
-- Fix {fixMark = 13103s, fix = LLA {llaLat = -33.65073300°, llaLng = 147.56036700°, llaAltGps = 214m}, fixAltBaro = Just 238m}
--
-- #range#
-- The length and range of the tracklog.
--
-- >>> fixesLength mf
-- 6547
-- >>> fixesSecondsRange mf
-- Just (0s,13103s)
-- >>> fixesUTCTimeRange mf
-- Just (2012-01-14 02:12:55 UTC,2012-01-14 05:51:18 UTC)
--
-- #showfixes#
-- Showing the fixes in the tracklog.
--
-- >>> showFixesLength mf
-- "6547"
-- >>> showFixesSecondsRange mf
-- "(0s,13103s)"
-- >>> showFixesUTCTimeRange mf
-- "(2012-01-14 02:12:55 UTC,2012-01-14 05:51:18 UTC)"
--
-- Showing a single fix.
--
-- >>> let a = head fixes
-- >>> let z = last fixes
-- >>> let lla = (lat . fix $ a, lng . fix $ a, altGps . fix $ a)
-- >>> showLatLngAlt lla
-- "-33.361600,147.932050,237"
-- >>> showLngLatAlt lla
-- "147.932050,-33.361600,237"
-- >>> showTimeAlt a
-- "(0s,237m)"
-- >>> showTimeAlt z
-- "(13103s,214m)"
