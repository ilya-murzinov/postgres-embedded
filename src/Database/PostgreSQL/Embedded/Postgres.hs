module Database.PostgreSQL.Embedded.Postgres
    ( startPostgres
    , stopPostgres
    , checkPostgresStarted
    ) where

import           Control.Concurrent                    (threadDelay)
import           Control.Exception                     (try)
import           Data.Conduit.Shell                    (rm, run, shell)
import           Data.List                             (isInfixOf)
import           Data.Monoid                           ((<>))
import           Database.HDBC                         (SqlError)
import           Database.HDBC.PostgreSQL              (Connection,
                                                        connectPostgreSQL)
import           System.FilePath.Posix                 ((</>))
import           System.Info                           (os)

import           Database.PostgreSQL.Embedded.Download
import           Database.PostgreSQL.Embedded.Types

startPostgres :: StartupConfig -> DBConfig -> IO RuntimeConfig
startPostgres (StartupConfig version_ t) dConfig@(DBConfig p u) = do
    e <- downloadPostgres getOS version_
    let d = e </> "data"
    run $ do
        shell $ e </> "bin" </> "initdb" <> " -A trust -U " <> u <> " -D " <> d <> " -E UTF-8"
        shell $ e </> "bin" </> "pg_ctl" <> " -D " <> d <> " -o \"-F -p " <>
            (show p) <> "\"" <> " -l " <> (e </> "log") <> " start"
    checkPostgresStarted dConfig t
    return $ RuntimeConfig e d
    where
        getOS | "darwin" `isInfixOf` os = OSX
              | "linux" `isInfixOf` os = Linux
              | "win" `isInfixOf` os = Win
              | otherwise = error $ "Unsupported platform" <> os

stopPostgres :: RuntimeConfig -> IO ()
stopPostgres (RuntimeConfig e d) = run $ do
        shell $ e </> "bin" </> "pg_ctl" <> " -D " <> d <> " stop"
        rm "-rf" d

checkPostgresStarted :: DBConfig -> Int -> IO ()
checkPostgresStarted config secs = checkPostgresStarted_ config secs >>= \_ -> return ()
    where
        checkPostgresStarted_ :: DBConfig -> Int -> IO Bool
        checkPostgresStarted_ (DBConfig p u) n = do
            let connStr = "postgres://" <> u <> "@localhost:" <> (show p) <> "/postgres"
            let oneSec = 1000000

            res <- try $ connectPostgreSQL connStr :: IO (Either SqlError Connection)
            case res of
                Left e -> if (n > 0) then
                    print e >>=
                        \_ -> threadDelay oneSec >>=
                            \_ -> checkPostgresStarted_ config (n - 1)
                else
                    return False
                Right _ -> return True
