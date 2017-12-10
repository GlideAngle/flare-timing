module Flight.Path
    ( FsdbFile(..)
    , FsdbXml(..)
    , CompFile(..)
    , CrossFile(..)
    , fsdbToComp
    ) where

import System.FilePath (replaceExtension)

-- | The path to a competition *.fsdb file.
newtype FsdbFile = FsdbFile FilePath

-- | The XML string contents of a *.fsdb file.
newtype FsdbXml = FsdbXml String

-- | The path to a competition file.
newtype CompFile = CompFile FilePath

-- | The path to a crossings file or cross zone file.
newtype CrossFile = CrossFile FilePath

fsdbToComp :: FsdbFile -> CompFile
fsdbToComp (FsdbFile p) =
    CompFile $ replaceExtension p ".comp-input.yaml"
