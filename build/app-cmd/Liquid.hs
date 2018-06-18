module Liquid (liquidRules) where

import Development.Shake
    ( Rules
    , CmdOption(Shell, Cwd)
    , phony
    , cmd
    )
import Development.Shake.FilePath ((</>))

liquidRules :: Rules ()
liquidRules = do
    phony "liquid-gap" $
        cmd
            Shell shell
    where
        shell =
            "stack exec liquid -- "
            ++ "\"--ghc-option=-XPackageImports\""
            ++ " "
            ++ "\"--ghc-option=-fplugin Data.UnitsOfMeasure.Plugin\""
            ++ " "
            ++ ("gap" </> "library" </> "Flight" </> "Gap" </> "Validity.hs")
            ++ " "
            ++ ("gap" </> "library" </> "Flight" </> "Gap" </> "Pilots.hs")
            ++ " "
            ++ ("gap" </> "library" </> "Flight" </> "Gap" </> "Time" </> "Best.hs")
            ++ " "
            ++ ("aeson-via-uom" </> "library" </> "Data" </> "Aeson" </> "Via" </> "UnitsOfMeasure.hs")
            ++ " "
            ++ ("units" </> "library" </> "Flight" </> "Units.hs")
            ++ " "
            ++ ("units" </> "library" </> "Flight" </> "Units" </> "Angle.hs")
            ++ " "
            ++ ("gap" </> "library" </> "Flight" </> "Gap" </> "Time" </> "Nominal.hs")
            ++ " "
            ++ ("gap" </> "library" </> "Flight" </> "Gap" </> "Ratio" </> "Goal.hs")
            ++ " "
            ++ ("gap" </> "library" </> "Flight" </> "Gap" </> "Ratio" </> "Launch.hs")
            ++ " "
            ++ ("gap" </> "library" </> "Flight" </> "Gap" </> "Validity" </> "Task.hs")
            ++ " "
            ++ ("gap" </> "library" </> "Flight" </> "Gap" </> "Validity" </> "Time.hs")
            ++ " "
            ++ ("gap" </> "library" </> "Flight" </> "Gap" </> "Validity" </> "Distance.hs")
            ++ " "
            ++ ("gap" </> "library" </> "Flight" </> "Gap" </> "Validity" </> "Launch.hs")
            ++ " "
            ++ ("gap" </> "library" </> "Flight" </> "Gap" </> "Validity" </> "Area.hs")
            ++ " "
            ++ ("gap" </> "library" </> "Flight" </> "Gap" </> "Distance" </> "Sum.hs")
            ++ " "
            ++ ("gap" </> "library" </> "Flight" </> "Gap" </> "Distance" </> "Max.hs")
            ++ " "
            ++ ("gap" </> "library" </> "Flight" </> "Gap" </> "Distance" </> "Min.hs")
            ++ " "
            ++ ("gap" </> "library" </> "Flight" </> "Gap" </> "Distance" </> "Best.hs")
            ++ " "
            ++ ("gap" </> "library" </> "Flight" </> "Gap" </> "Distance" </> "Nominal.hs")
            ++ " "
            ++ ("units" </> "library" </> "Flight" </> "Ratio.hs")
