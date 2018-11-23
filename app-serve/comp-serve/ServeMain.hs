import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import Network.Wai (Application)
import Network.Wai.Middleware.Cors (simpleCors)
import Network.Wai.Handler.Warp
    (runSettings, defaultSettings, setPort, setBeforeMainLoop)
import Servant
    ( (:<|>)(..)
    , Get, JSON, Server, Handler(..), Proxy(..), ServantErr
    , (:>)
    , err400, errBody, hoistServer, serve, throwError
    )
import System.IO (hPutStrLn, stderr)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT, MonadReader, asks, liftIO, runReaderT)
import Control.Monad.Trans.Except (throwE)
import Control.Monad.Except (ExceptT(..), MonadError, runExceptT, lift)
import qualified Data.ByteString.Lazy.Char8 as LBS (pack)

import System.FilePath (FilePath, takeFileName)
import Data.Yaml (prettyPrintParseException, decodeEither')
import qualified Data.ByteString as BS (readFile)

import Flight.Cmd.Paths (LenientFile(..), checkPaths)
import Flight.Cmd.Options (ProgramName(..))
import Flight.Cmd.ServeOptions (CmdServeOptions(..), mkOptions)
import Flight.Comp
    ( FileType(CompInput)
    , CompSettings(..)
    , Comp
    , Task
    , Nominal
    , PilotTrackLogFile(..)
    , Pilot(..)
    , CompInputFile(..)
    , findCompInput
    , ensureExt
    )
import ServeOptions (description)

newtype Config = Config {path :: FilePath}

newtype AppT m a =
    AppT
        { unApp :: ReaderT Config (ExceptT ServantErr m) a
        }
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadReader Config
        , MonadError ServantErr
        , MonadIO
        )

type Api =
    "comps" :> Get '[JSON] Comp
    :<|> "nominals" :> Get '[JSON] Nominal
    :<|> "tasks" :> Get '[JSON] [Task Double]
    :<|> "pilots" :> Get '[JSON] [[Pilot]]

api :: Proxy Api
api = Proxy

convertApp :: Config -> AppT IO a -> Handler a
convertApp cfg appt = Handler $ runReaderT (unApp appt) cfg

main :: IO ()
main = do
    name <- getProgName
    options <- cmdArgs $ mkOptions (ProgramName name) description Nothing

    let lf = LenientFile {coerceFile = ensureExt CompInput}
    err <- checkPaths lf options

    maybe (drive options) putStrLn err

drive :: CmdServeOptions -> IO ()
drive o = do
    files <- findCompInput o
    if null files then putStrLn "Couldn't find any input files."
                  else mapM_ (go o) files
go :: CmdServeOptions -> CompInputFile -> IO ()
go CmdServeOptions{..} (CompInputFile compPath) = do
    putStrLn $ "Reading competition from '" ++ takeFileName compPath ++ "'"
    runSettings settings =<< mkApp (Config compPath)
    where
        port = 3000

        settings =
            setPort port $
            setBeforeMainLoop
                (hPutStrLn stderr ("listening on port " ++ show port))
                defaultSettings

-- SEE: https://stackoverflow.com/questions/42143155/acess-a-servant-server-with-a-reflex-dom-client
mkApp :: Config -> IO Application
mkApp cfg = return . simpleCors . serve api $ serverApi cfg

serverApi :: Config -> Server Api
serverApi cfg =
    hoistServer
        api
        (convertApp cfg)
        ( query (yaml comp)
        :<|> query (yaml nominal)
        :<|> query (yaml tasks)
        :<|> query (yaml ((fmap . fmap) pilot . pilots))
        )
    where
        query f = do
            path' <- asks path
            xs <- liftIO . runExceptT . f $ path'
            case xs of
              Left msg -> throwError $ err400 { errBody = LBS.pack msg }
              Right xs' -> return xs'

        pilot (PilotTrackLogFile p _) = p

yaml :: (CompSettings Double -> a) -> FilePath -> ExceptT String IO a
yaml f yamlPath = do
    contents <- lift $ BS.readFile yamlPath
    case decodeEither' contents of
        Left msg -> throwE . prettyPrintParseException $ msg
        Right x -> ExceptT . return $ Right (f x)
