import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import System.FilePath (takeFileName)
import System.Directory (getCurrentDirectory)

import Flight.Cmd.Paths (LenientFile(..), checkPaths)
import Flight.Igc (parseFromFile)
import Flight.Comp (FindDirFile(..), IgcFile(..), findIgc)
import IgcOptions (IgcOptions(..), mkOptions)

main :: IO ()
main = do
    name <- getProgName
    options <- cmdArgs $ mkOptions name

    let lf = LenientFile {coerceFile = id}
    err <- checkPaths lf options

    maybe (drive options) putStrLn err

drive :: IgcOptions -> IO ()
drive IgcOptions{file} = do
    cwd <- getCurrentDirectory
    files <- findIgc $ FindDirFile {dir = cwd, file = file}
    if null files then putStrLn "Couldn't find any input files."
                  else mapM_ go files

go :: IgcFile -> IO ()
go (IgcFile path) = do
    putStrLn $ takeFileName path
    p <- parseFromFile path
    either print print p
