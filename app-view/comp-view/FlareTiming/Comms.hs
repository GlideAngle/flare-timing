module FlareTiming.Comms
    ( getComps
    , getNominals
    , getTasks
    , getPilots
    , getValidity
    , getAllocation
    , getTaskScore
    ) where

import Reflex
import Reflex.Dom
import qualified Data.Text as T (Text, pack)

import WireTypes.Comp (Comp(..), Nominal(..), Task(..))
import WireTypes.Pilot (Pilot(..))
import WireTypes.Track.Point
import FlareTiming.Events (IxTask(..))

-- NOTE: Possible alternatives for mapUri ...
-- mapUri s = "http://localhost:3000" <> s
-- mapUri s = "http://2012-forbes.flaretiming.com/json" <> s <> ".json"
-- mapUri s = "/json" <> s <> ".json"
mapUri :: T.Text -> T.Text
mapUri s = "/json" <> s <> ".json"

getTasks :: MonadWidget t m => () -> m (Dynamic t [Task])
getTasks () = do
    pb :: Event t () <- getPostBuild
    let defReq = mapUri "/tasks"
    let req md = XhrRequest "GET" (maybe defReq id md) def
    rsp <- performRequestAsync $ fmap req $ leftmost [ Nothing <$ pb ]

    let es :: Event t [Task] = fmapMaybe decodeXhrResponse rsp
    holdDyn [] es

getComps
    :: MonadWidget t m
    => ()
    -> m (Dynamic t [Comp])
getComps () = do
    pb :: Event t () <- getPostBuild
    let defReq = mapUri "/comps"
    let req md = XhrRequest "GET" (maybe defReq id md) def
    rsp <- performRequestAsync $ fmap req $ leftmost [ Nothing <$ pb ]

    let es :: Event t Comp = fmapMaybe decodeXhrResponse rsp
    xs :: Dynamic t [Comp] <- holdDyn [] (pure <$> es)
    return xs

getNominals
    :: MonadWidget t m
    => ()
    -> m (Dynamic t [Nominal])
getNominals () = do
    pb :: Event t () <- getPostBuild
    let defReq = mapUri "/nominals"
    let req md = XhrRequest "GET" (maybe defReq id md) def
    rsp <- performRequestAsync $ fmap req $ leftmost [ Nothing <$ pb ]

    let es :: Event t Nominal = fmapMaybe decodeXhrResponse rsp
    xs :: Dynamic t [Nominal] <- holdDyn [] (pure <$> es)
    return xs

getPilots
    :: MonadWidget t m
    => ()
    -> m (Dynamic t [Pilot])
getPilots () = do
    pb :: Event t () <- getPostBuild
    let defReq = mapUri "/pilots"
    let req md = XhrRequest "GET" (maybe defReq id md) def
    rsp <- performRequestAsync $ fmap req $ leftmost [ Nothing <$ pb ]

    let es :: Event t [Pilot] = fmapMaybe decodeXhrResponse rsp
    holdDyn [] es

getValidity
    :: MonadWidget t m
    => ()
    -> m (Dynamic t [Maybe Validity])
getValidity () = do
    pb :: Event t () <- getPostBuild
    let defReq = mapUri "/gap-point/validity"
    let req md = XhrRequest "GET" (maybe defReq id md) def
    rsp <- performRequestAsync $ fmap req $ leftmost [ Nothing <$ pb ]

    let es :: Event t [Maybe Validity] = fmapMaybe decodeXhrResponse rsp
    holdDyn [] es

getAllocation
    :: MonadWidget t m
    => ()
    -> m (Dynamic t [Maybe Allocation])
getAllocation () = do
    pb :: Event t () <- getPostBuild
    let defReq = mapUri "/gap-point/allocation"
    let req md = XhrRequest "GET" (maybe defReq id md) def
    rsp <- performRequestAsync $ fmap req $ leftmost [ Nothing <$ pb ]

    let es :: Event t [Maybe Allocation] = fmapMaybe decodeXhrResponse rsp
    holdDyn [] es

getTaskScore
    :: MonadWidget t m
    => IxTask
    -> m (Dynamic t [(Pilot, Breakdown)])

getTaskScore (IxTask ii) = do
    pb :: Event t () <- getPostBuild

    let defReq :: T.Text
        defReq =
            mapUri
            $ "/gap-point/"
            <> (T.pack . show $ ii)
            <> "/score"

    let req md = XhrRequest "GET" (maybe defReq id md) def
    rsp <- performRequestAsync $ fmap req $ leftmost [ Nothing <$ pb ]

    let es :: Event t [(Pilot, Breakdown)] = fmapMaybe decodeXhrResponse rsp
    holdDyn [] es

getTaskScore IxTaskNone = return $ constDyn []
