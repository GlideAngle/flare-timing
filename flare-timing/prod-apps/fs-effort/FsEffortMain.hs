import Data.Coerce (coerce)
import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import Formatting ((%), fprint)
import Formatting.Clock (timeSpecs)
import System.Clock (getTime, Clock(Monotonic))
import Control.Monad (mapM_)
import Control.Monad.Except (ExceptT(..), runExceptT, lift)
import Data.UnitsOfMeasure (u)
import Data.UnitsOfMeasure.Internal (Quantity(..))

import Flight.LatLng (LatLng(..), Lat(..), Lng(..))
import Flight.LatLng.Raw (RawLatLng(..), RawLat(..), RawLng(..))
import Flight.Cmd.Paths (LenientFile(..), checkPaths)
import Flight.Cmd.Options (ProgramName(..))
import Flight.Cmd.BatchOptions (CmdBatchOptions(..), mkOptions)
import Flight.Fsdb (parseNormRoutes)
import Flight.Comp
    ( FileType(Fsdb)
    , FsdbFile(..)
    , FsdbXml(..)
    , fsdbToNormRoute
    , findFsdb
    , ensureExt
    )
import Flight.Route
import Flight.TaskTrack.Double
import Flight.Scribe (writeNormRoute)
import FsEffortOptions (description)

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
    settings <- runExceptT $ normRoutes (FsdbXml contents')
    either print (writeNormRoute (fsdbToNormRoute fsdbFile)) settings

fsdbRoutes :: FsdbXml -> ExceptT String IO [[RawLatLng]]
fsdbRoutes (FsdbXml contents) = do
    fs <- lift $ parseNormRoutes contents
    let fs' = (fmap . fmap . fmap) convertLatLng fs
    ExceptT $ return fs'

normRoutes :: FsdbXml -> ExceptT String IO [GeoLines]
normRoutes fsdbXml = do
    rs <- fsdbRoutes fsdbXml
    let rs' = fmap (geoTrack False) rs
    return rs'

convertLatLng :: LatLng Rational [u| deg |] -> RawLatLng
convertLatLng (LatLng (Lat lat, Lng lng)) =
    RawLatLng{lat = RawLat $ coerce lat, lng = RawLng $ coerce lng}
