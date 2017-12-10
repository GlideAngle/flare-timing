module Flight.Path (CompFile(..), CrossFile(..)) where

-- | The path to a competition file.
newtype CompFile = CompFile FilePath

-- | The path to a crossings file or cross zone file.
newtype CrossFile = CrossFile FilePath
