{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Servant.PY
import Serve.Api

main :: IO ()
main = do
    writePythonForAPI compInputApi requests "py-client/comp-input.py"
    writePythonForAPI taskLengthApi requests "py-client/task-length.py"
    writePythonForAPI gapPointApi requests "py-client/gap-point.py"
