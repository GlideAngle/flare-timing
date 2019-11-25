import System.Environment (getProgName)
import System.Console.CmdArgs.Implicit (cmdArgs)
import Formatting ((%), fprint)
import Formatting.Clock (timeSpecs)
import System.Clock (getTime, Clock(Monotonic))
import Control.Monad (mapM_)
import System.FilePath (takeFileName)

import Flight.Cmd.Paths (LenientFile(..), checkPaths)
import Flight.Comp
    ( FileType(CompInput)
    , CompSettings(..)
    , Comp(..)
    , Task(zones, speedSection)
    , CompInputFile(..)
    , compToTaskLength
    , findCompInput
    , ensureExt
    )
import Flight.TaskTrack.Double (taskTracks)
import Flight.Scribe (readComp, writeRoute)
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

    let lf = LenientFile {coerceFile = ensureExt CompInput}
    err <- checkPaths lf options

    maybe (drive options) putStrLn err

drive :: CmdOptions -> IO ()
drive o = do
    -- SEE: http://chrisdone.com/posts/measuring-duration-in-haskell
    start <- getTime Monotonic
    files <- findCompInput o
    if null files then putStrLn "Couldn't find any input files."
                  else mapM_ (go o) files
    end <- getTime Monotonic
    fprint ("Measuring task lengths completed in " % timeSpecs % "\n") start end

go :: CmdOptions -> CompInputFile -> IO ()
go CmdOptions{..} compFile@(CompInputFile compPath) = do
    putStrLn $ takeFileName compPath

    settings <- readComp compFile
    f settings
    where
        f cs@CompSettings{comp = Comp{earthMath = eMath, give}}= do
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

            let zss = unlineZones az . unkindZones give . zones <$> tasks cs
            let includeTask = if null task then const True else flip elem task

            writeRoute
                (compToTaskLength compFile)
                (taskTracks sp noTaskWaypoints includeTask measure ixs zss)
