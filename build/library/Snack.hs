module Snack (buildRules) where

import Development.Shake
    ( Rules
    , CmdOption(Shell, Cwd)
    , phony
    , cmd
    , need
    )

buildRules :: Rules ()
buildRules = do
    phony "snack" $
        need [ "snack-siggy-chardust"
             ]

    phony "snack-siggy-chardust" $
        cmd
            (Cwd "siggy-chardust")
            Shell "snack build"
