module Database.PostgreSQL.Embedded.Types
    ( Version(..)
    , Os(..)
    , StartupConfig(..)
    , RuntimeConfig(..)
    , DBConfig(..)
    ) where

-- | Type of operation system
-- Doesn't really work for Windows now
data Os = Win | OSX | Linux deriving (Eq)

-- | Version of PostgreSQL distribution
-- See https://www.enterprisedb.com/downloads/postgres-postgresql-downloads for supported versions
newtype Version = Version { value :: String }

-- | Config for stating up instance
-- Note that startup requeres clean or non-existing directory
data StartupConfig = StartupConfig
    { -- | Should data directory be cleaned up?
      cleanDir :: Bool
      -- | See @Version@
    , version  :: Version
    }

-- | Config of running instance
data RuntimeConfig = RuntimeConfig
    { -- | Path of extracter PostgreSQL distribution
      execDir :: FilePath
      -- | Data directory
    , dataDir :: FilePath
    }

-- | Database config
data DBConfig = DBConfig
    { -- | Port
      port     :: Integer
      -- | Username
    , username :: String
    }
