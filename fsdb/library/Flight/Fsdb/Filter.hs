module Flight.Fsdb.Filter (filterComp) where

import Flight.Comp

filterComp :: FsdbXml -> IO (Either String FsdbXml)
filterComp x@(FsdbXml _) = do
    return $ Right x
