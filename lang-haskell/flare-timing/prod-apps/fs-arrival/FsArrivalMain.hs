import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import Formatting ((%), fprint)
import Formatting.Clock (timeSpecs)
import System.Clock (getTime, Clock(Monotonic))
import Control.Monad (mapM_)
import Control.Monad.Trans.Except (throwE)
import Control.Monad.Except (ExceptT(..), runExceptT, lift)

import Flight.Cmd.Paths (LenientFile(..), checkPaths)
import Flight.Cmd.Options (ProgramName(..))
import Flight.Cmd.BatchOptions (CmdBatchOptions(..), mkOptions)
import Flight.Fsdb (parseNominal, parseNormEfforts)
import Flight.Track.Land (Landing(..), TaskLanding(..), compLanding)
import Flight.Comp
    ( FileType(TrimFsdb)
    , TrimFsdbFile(..)
    , FsdbXml(..)
    , Nominal(..)
    , trimFsdbToNormEffort
    , findTrimFsdb
    , ensureExt
    )
import Flight.Scribe (readTrimFsdb, writeNormEffort)
import FsArrivalOptions (description)

main :: IO ()
main = do
    name <- getProgName
    options <- cmdArgs $ mkOptions (ProgramName name) description Nothing

    let lf = LenientFile {coerceFile = ensureExt TrimFsdb}
    err <- checkPaths lf options

    maybe (drive options) putStrLn err

drive :: CmdBatchOptions -> IO ()
drive o = do
    -- SEE: http://chrisdone.com/posts/measuring-duration-in-haskell
    start <- getTime Monotonic
    files <- findTrimFsdb o

    if null files then putStrLn "Couldn't find any input files."
                  else mapM_ go files
    end <- getTime Monotonic
    fprint ("Extracting expected or normative arrivals completed in " % timeSpecs % "\n") start end

go :: TrimFsdbFile -> IO ()
go trimFsdbFile = do
    FsdbXml contents <- readTrimFsdb trimFsdbFile
    let contents' = dropWhile (/= '<') contents
    settings <- runExceptT $ normEfforts (FsdbXml contents')
    either print (writeNormEffort (trimFsdbToNormEffort trimFsdbFile)) settings

fsdbNominal :: FsdbXml -> ExceptT String IO Nominal
fsdbNominal (FsdbXml contents) = do
    ns <- lift $ parseNominal contents
    case ns of
        Left msg -> ExceptT . return $ Left msg
        Right [n] -> ExceptT . return $ Right n
        _ -> do
            let msg = "Expected only one set of nominals for the comp"
            lift $ print msg
            throwE msg

fsdbEfforts :: FsdbXml -> ExceptT String IO [TaskLanding]
fsdbEfforts (FsdbXml contents) = do
    fs <- lift $ parseNormEfforts contents
    ExceptT $ return fs

normEfforts :: FsdbXml -> ExceptT String IO Landing
normEfforts fsdbXml = do
    Nominal{free} <- fsdbNominal fsdbXml
    es <- fsdbEfforts fsdbXml
    return $ compLanding free es
