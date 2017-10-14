-- | Library for easily running embedded PostgreSQL server for tests

module Database.PostgreSQL.Embedded
    ( startPostgres
    , stopPostgres
    , Version(..)
    , Os(..)
    , StartupConfig(..)
    , RuntimeConfig(..)
    , DBConfig(..)
    ) where

import           Database.PostgreSQL.Embedded.Postgres
import           Database.PostgreSQL.Embedded.Types
