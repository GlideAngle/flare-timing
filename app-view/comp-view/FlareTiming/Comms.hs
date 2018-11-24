module FlareTiming.Comms
    ( getComps
    , getNominals
    , getTasks
    , getPilots
    ) where

import Reflex
import Reflex.Dom

import Data.Flight.Types (Comp(..), Nominal(..), Pilot(..), Task(..))

getTasks :: MonadWidget t m => () -> m (Dynamic t [Task])
getTasks () = do
    pb :: Event t () <- getPostBuild
    let defReq = "http://localhost:3000/tasks"
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
    let defReq = "http://localhost:3000/comps"
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
    let defReq = "http://localhost:3000/nominals"
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
    let defReq = "http://localhost:3000/pilots"
    let req md = XhrRequest "GET" (maybe defReq id md) def
    rsp <- performRequestAsync $ fmap req $ leftmost [ Nothing <$ pb ]

    let es :: Event t [Pilot] = fmapMaybe decodeXhrResponse rsp
    holdDyn [] es
