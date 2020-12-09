module Flight.Fsdb.TaskRoute (parseAltRoutes) where

import Data.Maybe (catMaybes)
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Text.XML.HXT.Arrow.Pickle
    ( XmlPickler(..), PU(..)
    , xpickle, unpickleDoc, xpWrap, xpFilterAttr, xpElem, xpAttr, xpPair
    )
import Text.XML.HXT.DOM.TypeDefs (XmlTree)
import Text.XML.HXT.Core
    ( ArrowXml
    , (<+>)
    , (>>>)
    , runX
    , withValidate
    , withWarnings
    , readString
    , no
    , hasName
    , getChildren
    , listA
    , arr
    , deep
    )

import Flight.Units ()
import Flight.LatLng (LatLng(..), Lat(..), Lng(..))
import Flight.LatLng.Raw (RawLat(..), RawLng(..))
import Flight.Fsdb.Internal.XmlPickle ()

xpWaypoint :: PU (LatLng Rational [u| deg |])
xpWaypoint =
    xpElem "FsPathVertex"
    $ xpFilterAttr (hasName "lat" <+> hasName "lon")
    $ xpWrap
        ( \(RawLat lat, RawLng lng) ->
            LatLng (Lat . MkQuantity $ lat, Lng . MkQuantity $ lng)
        , \(LatLng (Lat (MkQuantity lat), Lng (MkQuantity lng))) ->
            (RawLat lat, RawLng lng)
        )
    $ xpPair
        (xpAttr "lat" xpickle)
        (xpAttr "lon" xpickle)

getRoute :: ArrowXml a => a XmlTree [LatLng Rational [u| deg |]]
getRoute =
    getChildren
    >>> deep (hasName "FsTask")
    >>> getWaypoints
    where
        getWaypoints =
            getChildren
            >>> hasName "FsTaskShortestPath"
            >>> (listA getTps >>> arr catMaybes)
            where
                getTps =
                    getChildren
                    >>> hasName "FsPathVertex"
                    >>> arr (unpickleDoc xpWaypoint)


parseAltRoutes :: String -> IO (Either String [[LatLng Rational [u| deg |]]])
parseAltRoutes contents = do
    let doc = readString [ withValidate no, withWarnings no ] contents
    xs <- runX $ doc >>> getRoute
    return $ Right xs
