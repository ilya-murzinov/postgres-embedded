module Database.PostgreSQL.Embedded.Types
    ( Version(..)
    , Os(..)
    , StartupConfig(..)
    , RuntimeConfig(..)
    , DBConfig(..)
    ) where

data Os = Win | OSX | Linux deriving (Eq)

newtype Version = Version { value :: String }

data StartupConfig = StartupConfig {
    version        :: Version,
    startupTimeout :: Int
}

data RuntimeConfig = RuntimeConfig {
    execDir :: FilePath,
    dataDir :: FilePath
}

data DBConfig = DBConfig {
    port     :: Int,
    username :: String
}
