import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import Formatting ((%), fprint)
import Formatting.Clock (timeSpecs)
import System.Clock (getTime, Clock(Monotonic))
import Control.Monad (mapM_)
import Control.Monad.Except (ExceptT(..), runExceptT, lift)

import Flight.Cmd.Paths (LenientFile(..), checkPaths)
import Flight.Cmd.Options (ProgramName(..))
import Flight.Cmd.BatchOptions (CmdBatchOptions(..), mkOptions)
import qualified Flight.Fsdb as Fsdb (filterComp)
import Flight.Comp
    ( FileType(Fsdb)
    , FsdbFile(..)
    , FsdbXml(..)
    , fsdbToFilterFsdb
    , findFsdb
    , ensureExt
    )
import Flight.Scribe (writeFilterFsdb)
import FsFilterOptions (description)

main :: IO ()
main = do
    name <- getProgName
    options <- cmdArgs $ mkOptions (ProgramName name) description Nothing

    let lf = LenientFile {coerceFile = ensureExt Fsdb}
    err <- checkPaths lf options

    maybe (drive options) putStrLn err

drive :: CmdBatchOptions -> IO ()
drive o = do
    -- SEE: http://chrisdone.com/posts/measuring-duration-in-haskell
    start <- getTime Monotonic
    files <- findFsdb o

    if null files then putStrLn "Couldn't find any input files."
                  else mapM_ go files
    end <- getTime Monotonic
    fprint ("Extracting expected or normative optimal routes completed in " % timeSpecs % "\n") start end

go :: FsdbFile -> IO ()
go fsdbFile@(FsdbFile fsdbPath) = do
    contents <- readFile fsdbPath
    let contents' = dropWhile (/= '<') contents
    settings <- runExceptT $ filterComp (FsdbXml contents')
    either print (writeFilterFsdb (fsdbToFilterFsdb fsdbFile)) settings

filterComp :: FsdbXml -> ExceptT String IO FsdbXml
filterComp fsdbXml = do
    x <- lift $ Fsdb.filterComp fsdbXml
    ExceptT $ return x
