import           Data.ByteString.Char8        (pack)
import           Data.Monoid                  ((<>))
import           Data.String                  (fromString)
import           Database.PostgreSQL.Simple   (Only (..), connectPostgreSQL,
                                               query_)
import           System.FilePath.Posix        ((</>))
import           System.Process               (system)

import           Database.PostgreSQL.Embedded

main :: IO ()
main = do
    let sConfig = StartupConfig True (Version "9.6.5-1")
    let dConfig = DBConfig 46782 "postgres"

    system $ "rm" <> " -rf " <> ("~/.postgres-embedded/" </> "9.6.5-1")

    -- Start Postgres downloading distribution
    rc <- startPostgres sConfig dConfig
    runQuery dConfig
    stopPostgres rc

    -- Start Postgres with cached distribution
    rc1 <- startPostgres sConfig dConfig
    runQuery dConfig
    stopPostgres rc1

runQuery :: DBConfig -> IO ()
runQuery (DBConfig p u) = do
    conn <- connectPostgreSQL $ pack $ "host=127.0.0.1 port=" <> show p <> " user=" <> u
    _ <- (query_ conn $ fromString "select 1") :: IO [Only Int]
    return ()
