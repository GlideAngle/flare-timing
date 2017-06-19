{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module Serve.Driver (driverRun) where

import Control.Monad.Trans.Except
import Data.Aeson
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.IO

type ItemApi
    = "item" :> Get '[JSON] [Item]
    :<|> "item" :> Capture "itemId" Integer :> Get '[JSON] Item

itemApi :: Proxy ItemApi
itemApi = Proxy

driverRun :: IO ()
driverRun = do
    let port = 3000
        settings =
            setPort port $
            setBeforeMainLoop
                (hPutStrLn stderr ("listening on port " ++ show port))
                defaultSettings
    runSettings settings =<< mkApp

mkApp :: IO Application
mkApp = return $ serve itemApi server

server :: Server ItemApi
server
    = getItems
    :<|> getItemById

getItems :: Handler [Item]
getItems = return [exampleItem]

getItemById :: Integer -> Handler Item
getItemById = \ case
    0 -> return exampleItem
    _ -> throwE err404

exampleItem :: Item
exampleItem = Item { itemId = 0, itemText = "some item" }

data Item = Item { itemId :: Integer
                 , itemText :: String
                 } deriving (Eq, Show, Generic)

instance ToJSON Item
instance FromJSON Item
