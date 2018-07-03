module Flight.Fsdb.Comp (parseComp) where

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

import Data.Bifunctor (bimap)
import Text.Megaparsec ((<?>))

import Flight.Comp (Comp(..), UtcOffset(..))
import Flight.Score (Discipline(..))
import Flight.Fsdb.Internal (prs, sci, sciToInt)

getComp :: ArrowXml a => a XmlTree (Either String Comp)
getComp =
    getChildren
    >>> deep (hasName "FsCompetition")
    >>> getAttrValue "id"
    &&& getAttrValue "name"
    &&& getAttrValue "discipline"
    &&& getAttrValue "location"
    &&& getAttrValue "from"
    &&& getAttrValue "to"
    &&& getUtcOffset
    >>> arr (\(i, (n, (d', (l, (f, (t, u)))))) ->
        let d =
                case d' of
                    "pg" -> Paragliding
                    _ -> HangGliding
        in (\u' -> Comp i n d l f t u' Nothing) <$> u)
    where
        getUtcOffset =
            getAttrValue "utc_offset"
            >>> arr parseUtcOffset

parseUtcOffset :: String -> Either String UtcOffset
parseUtcOffset s =
    bimap show (UtcOffset . toMins) hrs'
    where
        hrs = prs (sci <?> "Seconds of UTC offset") s
        toMins h = fromIntegral (h * 60)
        hrs' = sciToInt <$> hrs

parseComp :: String -> IO (Either String [Comp])
parseComp contents = do
    let doc = readString [ withValidate no, withWarnings no ] contents
    xs <- runX $ doc >>> getComp
    return $ sequence xs

