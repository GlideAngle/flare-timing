import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import Formatting ((%), fprint)
import Formatting.Clock (timeSpecs)
import System.Clock (getTime, Clock(Monotonic))
import Control.Monad (mapM_)
import System.FilePath (takeFileName)
import System.Directory (getCurrentDirectory)
import Control.Exception.Safe (catchIO)

import Flight.Cmd.Paths (LenientFile(..), checkPaths)
import Flight.Comp
    ( FindDirFile(..)
    , FileType(CompInput)
    , CompTaskSettings(..)
    , Comp(..)
    , Task(zones, speedSection)
    , CompInputFile(..)
    , compToTaskLength
    , findCompInput
    , reshape
    , mkCompTaskSettings
    )
import Flight.TaskTrack.Double (taskTracks)
import Flight.Scribe (readCompAndTasks, compFileToTaskFiles, writeRoute)
import Flight.Zone.MkZones (unkindZones)
import Flight.Zone (unlineZones)
import Flight.Zone.Cylinder (SampleParams(..), Samples(..), Tolerance(..))
import Flight.Earth.Ellipsoid (wgs84)
import Flight.Earth.Sphere (earthRadius)
import Flight.Geodesy (EarthMath(..), EarthModel(..), Projection(..))
import Flight.Geodesy.Solution (GeodesySolutions(..))
import TaskLengthOptions (CmdOptions(..), mkOptions)

sp :: SampleParams Double
sp = SampleParams (replicate 6 $ Samples 11) (Tolerance 0.03)

main :: IO ()
main = do
    name <- getProgName
    options <- cmdArgs $ mkOptions name

    let lf = LenientFile {coerceFile = reshape CompInput}
    err <- checkPaths lf options

    maybe (drive options) putStrLn err

drive :: CmdOptions -> IO ()
drive o@CmdOptions{file} = do
    -- SEE: http://chrisdone.com/posts/measuring-duration-in-haskell
    start <- getTime Monotonic
    cwd <- getCurrentDirectory
    files <- findCompInput $ FindDirFile {dir = cwd, file = file}
    if null files then putStrLn "Couldn't find any input files."
                  else mapM_ (go o) files
    end <- getTime Monotonic
    fprint ("Measuring task lengths completed in " % timeSpecs % "\n") start end

go :: CmdOptions -> CompInputFile -> IO ()
go CmdOptions{..} compFile@(CompInputFile compPath) = do
    putStrLn $ takeFileName compPath

    filesTaskAndSettings <-
        catchIO
            (Just <$> do
                ts <- compFileToTaskFiles compFile
                s <- readCompAndTasks (compFile, ts)
                return (ts, s))
            (const $ return Nothing)

    case filesTaskAndSettings of
        Nothing -> putStrLn "Couldn't read the comp settings."
        Just (_taskFiles, settings) -> f (uncurry mkCompTaskSettings $ settings)
    where
        f cs@CompTaskSettings{comp = Comp{earthMath = eMath}} = do
            let ixs = speedSection <$> tasks cs

            let az =
                    azimuthFwd @Double @Double
                        ( eMath
                        , let e = EarthAsEllipsoid wgs84 in case earthMath of
                              Pythagorus -> EarthAsFlat UTM
                              Haversines -> EarthAsSphere earthRadius
                              Vincenty -> e
                              AndoyerLambert -> e
                              ForsytheAndoyerLambert -> e
                              FsAndoyer -> e
                        )

            -- TODO: Find out if the give that enlarges zones is allowed to shorten
            -- the task length.
            let zss = unlineZones az . unkindZones Nothing. zones <$> tasks cs
            let includeTask = if null task then const True else flip elem task

            writeRoute
                (compToTaskLength compFile)
                (taskTracks sp noTaskWaypoints includeTask measure ixs zss)
