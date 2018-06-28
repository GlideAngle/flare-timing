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
            ++ " "
            ++ ("aeson-via-sci" </> "library" </> "Data" </> "Aeson" </> "Via" </> "Scientific.hs")
            ++ " "
            ++ ("uom-plugin-0.3.0.0" </> "src" </> "Data" </> "UnitsOfMeasure" </> "Convert.hs")
            ++ " "
            ++ ("uom-plugin-0.3.0.0" </> "src" </> "Data" </> "UnitsOfMeasure" </> "Singleton.hs")
            ++ " "
            ++ ("uom-plugin-0.3.0.0" </> "src" </> "Data" </> "UnitsOfMeasure" </> "Internal.hs")
            ++ " "
            ++ ("uom-plugin-0.3.0.0" </> "src" </> "Data" </> "UnitsOfMeasure.hs")
            ++ " "
            ++ ("uom-plugin-0.3.0.0" </> "src" </> "Data" </> "UnitsOfMeasure" </> "TH.hs")
            ++ " "
            ++ ("uom-plugin-0.3.0.0" </> "src" </> "Data" </> "UnitsOfMeasure" </> "Show.hs")
            ++ " "
            ++ ("uom-plugin-0.3.0.0" </> "src" </> "Data" </> "UnitsOfMeasure" </> "Read.hs")
            ++ " "
            ++ ("siggy-chardust" </> "library" </> "Data" </> "Number" </> "RoundingFunctions.hs")
            ++ " "
            ++ ("uom-plugin-0.3.0.0" </> "src" </> "Data" </> "UnitsOfMeasure" </> "Plugin.hs")
            ++ " "
            ++ ("ghc-8.2.2" </> "types" </> "TyCoRep.hs")
