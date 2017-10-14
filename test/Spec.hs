import           Database.PostgreSQL.Embedded

main :: IO ()
main = do
    let sConfig = StartupConfig True (Version "9.6.5-1") 10
    let dConfig = DBConfig 46782 "postgres"

    -- Start Postgres downloading distribution
    rc <- startPostgres sConfig dConfig
    stopPostgres rc

    -- Start Postgres with cached distribution
    rc1 <- startPostgres sConfig dConfig
    stopPostgres rc1
