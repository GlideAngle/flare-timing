import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import Formatting ((%), fprint)
import Formatting.Clock (timeSpecs)
import System.Clock (getTime, Clock(Monotonic))
import Control.Monad (mapM_)
import Control.Monad.Trans.Except (throwE)
import Control.Monad.Except (ExceptT(..), runExceptT, lift)
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import System.Directory (getCurrentDirectory)

import Flight.Cmd.Paths (LenientFile(..), checkPaths)
import Flight.Cmd.Options (ProgramName(..))
import Flight.Cmd.BatchOptions (CmdBatchOptions(..), mkOptions)
import Flight.Fsdb (parseNominal, parseAltLandouts)
import Flight.Track.Land (CompLanding(..), TaskLanding(..), mkCompLandOut)
import Flight.Comp
    ( AltDot(AltFs)
    , FindDirFile(..)
    , FileType(TrimFsdb)
    , TrimFsdbFile(..)
    , FsdbXml(..)
    , Nominal(..)
    , trimFsdbToAltLandout
    , findTrimFsdb
    , reshape
    )
import Flight.Score (MinimumDistance(..))
import Flight.Scribe (readTrimFsdb, writeAltLandOut)
import FsEffortOptions (description)

main :: IO ()
main = do
    name <- getProgName
    options <- cmdArgs $ mkOptions (ProgramName name) description Nothing

    let lf = LenientFile {coerceFile = reshape TrimFsdb}
    err <- checkPaths lf options

    maybe (drive options) putStrLn err

drive :: CmdBatchOptions -> IO ()
drive CmdBatchOptions{file} = do
    -- SEE: http://chrisdone.com/posts/measuring-duration-in-haskell
    start <- getTime Monotonic
    cwd <- getCurrentDirectory
    files <- findTrimFsdb $ FindDirFile {dir = cwd, file = file}

    if null files then putStrLn "Couldn't find any input files."
                  else mapM_ go files
    end <- getTime Monotonic
    fprint ("Extracting expected or normative efforts completed in " % timeSpecs % "\n") start end

go :: TrimFsdbFile -> IO ()
go trimFsdbFile = do
    FsdbXml contents <- readTrimFsdb trimFsdbFile
    let contents' = dropWhile (/= '<') contents
    settings <- runExceptT $ normEfforts (FsdbXml contents')
    either print (writeAltLandOut (trimFsdbToAltLandout AltFs trimFsdbFile)) settings

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

fsdbEfforts
    :: MinimumDistance (Quantity Double [u| km |])
    -> FsdbXml
    -> ExceptT String IO [TaskLanding]
fsdbEfforts free (FsdbXml contents) = do
    fs <- lift $ parseAltLandouts free contents
    ExceptT $ return fs

normEfforts :: FsdbXml -> ExceptT String IO CompLanding
normEfforts fsdbXml = do
    Nominal{free} <- fsdbNominal fsdbXml
    es <- fsdbEfforts free fsdbXml
    return $ mkCompLandOut es
