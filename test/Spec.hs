import           Database.PostgreSQL.Embedded
import           Database.PostgreSQL.Embedded

main :: IO ()
main = do
    let sConfig = StartupConfig OSX (Version "9.6.5-1") 10
    let dConfig = DBConfig 5432 "postgres"
    rc <- startPostgres sConfig dConfig
    stopPostgres rc
