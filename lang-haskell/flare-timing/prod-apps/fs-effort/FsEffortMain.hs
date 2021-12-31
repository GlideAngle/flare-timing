{-# LANGUAGE CPP #-}

import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import Formatting ((%), fprint)
import Formatting.Clock (timeSpecs)
import System.Clock (getTime, Clock(Monotonic))
#if __GLASGOW_HASKELL__ <= 802
import Control.Monad (mapM_)
#endif
import Control.Monad.Except (ExceptT(..), runExceptT, lift)
import System.Directory (getCurrentDirectory)

import Flight.Cmd.Paths (LenientFile(..), checkPaths)
import Flight.Cmd.Options (ProgramName(..))
import Flight.Cmd.BatchOptions (CmdBatchOptions(..), mkOptions)
import Flight.Fsdb (parseAltLandouts)
import Flight.Track.Land (CompLanding(..), TaskLanding(..), mkCompLandOut)
import Flight.Comp
    ( AltDot(AltFs)
    , FindDirFile(..)
    , FileType(TrimFsdb)
    , TrimFsdbFile(..)
    , FsdbXml(..)
    , trimFsdbToAltLandout
    , findTrimFsdb
    , reshape
    )
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

fsdbEfforts :: FsdbXml -> ExceptT String IO [TaskLanding]
fsdbEfforts (FsdbXml contents) = do
    fs <- lift $ parseAltLandouts contents
    ExceptT $ return fs

normEfforts :: FsdbXml -> ExceptT String IO CompLanding
normEfforts fsdbXml = do
    es <- fsdbEfforts fsdbXml
    return $ mkCompLandOut es
