module Flight.Cmd.Options
    ( ProgramName(..)
    , Description(..)
    , Extension(..)
    ) where

newtype ProgramName = ProgramName String
newtype Description = Description String
newtype Extension = Extension String
