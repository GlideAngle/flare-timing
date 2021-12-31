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
import qualified Flight.Fsdb as Fsdb (cleanComp)
import Flight.Comp
    ( FindDirFile(..)
    , FileType(Fsdb)
    , FsdbFile(..)
    , FsdbXml(..)
    , fsdbToCleanFsdb
    , findFsdb
    , reshape
    )
import Flight.Scribe (writeCleanFsdb)
import FsCleanOptions (description)

main :: IO ()
main = do
    name <- getProgName
    options <- cmdArgs $ mkOptions (ProgramName name) description Nothing

    let lf = LenientFile {coerceFile = reshape Fsdb}
    err <- checkPaths lf options

    maybe (drive options) putStrLn err

drive :: CmdBatchOptions -> IO ()
drive CmdBatchOptions{file} = do
    -- SEE: http://chrisdone.com/posts/measuring-duration-in-haskell
    start <- getTime Monotonic
    cwd <- getCurrentDirectory
    files <- findFsdb $ FindDirFile {dir = cwd, file = file}

    if null files then putStrLn "Couldn't find any input files."
                  else mapM_ go files
    end <- getTime Monotonic
    fprint ("Read *.fsdb, wrote it clean in " % timeSpecs % "\n") start end

go :: FsdbFile -> IO ()
go fsdbFile@(FsdbFile fsdbPath) = do
    rawContents <- readFile fsdbPath
    let rawXml = dropWhile (/= '<') rawContents

    cleanXml :: Either String FsdbXml <- runExceptT $ cleanComp (FsdbXml rawXml)
    either print (writeCleanFsdb (fsdbToCleanFsdb fsdbFile)) cleanXml

cleanComp :: FsdbXml -> ExceptT String IO FsdbXml
cleanComp fsdbXml = do
    x <- lift $ Fsdb.cleanComp fsdbXml
    ExceptT $ return x
