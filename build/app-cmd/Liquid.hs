module Liquid (liquidRules) where

import Development.Shake
    ( Rules
    , CmdOption(Shell, Cwd)
    , phony
    , cmd
    )

liquidRules :: Rules ()
liquidRules = do
    phony "liquid-gap" $
        cmd
            Shell shell
    where
        shell = "stack exec liquid -- --help"
