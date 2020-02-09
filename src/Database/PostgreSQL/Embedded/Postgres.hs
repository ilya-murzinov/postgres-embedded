module Database.PostgreSQL.Embedded.Postgres
    ( startPostgres
    , stopPostgres
    , checkPostgresStarted
    ) where

import           Control.Exception                     (SomeException, try)
import           System.Process                        (system)
import           Data.List                             (isInfixOf)
import           Data.Monoid                           ((<>))
import           System.Directory                      (doesDirectoryExist)
import           System.Exit                           (exitFailure)
import           System.FilePath.Posix                 ((</>))
import           System.Info                           (os)

import           Database.PostgreSQL.Embedded.Download
import           Database.PostgreSQL.Embedded.Types
import           Database.PostgreSQL.Embedded.Exec

{-|
Starts PostgreSQL instance with given config.

Returns @RuntimeConfig@ that is required for @stopPostgres@.
-}
startPostgres :: StartupConfig -> DBConfig -> IO RuntimeConfig
startPostgres (StartupConfig сlean version_ silent_) dConfig@(DBConfig p u) = do
    let execSystem = if silent_ then systemSilent else system
    e <- downloadPostgres silent_ getOS version_
    let d = e </> "data"
    exists <- doesDirectoryExist d
    _ <- if exists && not сlean then do
        putStrLn $ "Directory " <> d <> " must not exist"
        exitFailure
    else execSystem $ "rm" <> " -rf " <> d

    _ <- do
        _ <- execSystem $ e </> "bin" </> "initdb" <> " -A trust -U " <> u <> " -D " <> d <> " -E UTF-8"
        execSystem $ e </> "bin" </> "pg_ctl" <> " -w " <> " -D " <> d <> " -o \"-F -p " <>
            show p <> "\"" <> " -l " <> (e </> "log") <> " start"

    let r = RuntimeConfig e d silent_
    checkPostgresStarted r dConfig
    return r

    where
        getOS | "darwin" `isInfixOf` os = OSX
              | "linux" `isInfixOf` os = Linux
              | "win" `isInfixOf` os = Win
              | otherwise = error $ "Unsupported platform " <> os

{-|
Stops PostgreSQL instance.

Doesn't remove data directory.
-}
stopPostgres :: RuntimeConfig -> IO ()
stopPostgres (RuntimeConfig e d s) = do
        let execSystem = if s then systemSilent else system
        _ <- execSystem $ e </> "bin" </> "pg_ctl" <> " -D " <> d <> " stop" <> " -m fast -t 5 -w"
        return ()

checkPostgresStarted :: RuntimeConfig -> DBConfig -> IO ()
checkPostgresStarted (RuntimeConfig e _ s) (DBConfig p _) = checkPostgresStarted_ 10 >>= \_ -> return ()
    where
        checkPostgresStarted_ :: Int -> IO Bool
        checkPostgresStarted_ n = do
            let execSystem = if s then systemSilent else system
            let check = execSystem (e </> "bin" </> "pg_isready" <> " -p " <> show p <> " -h localhost") >>= \_ -> return True

            res <- try check :: IO (Either SomeException Bool)
            let retry = if n > 0 then checkPostgresStarted_ (n - 1)
                        else return False
            case res of
                Left err   -> print err >>= const retry
                Right res1 -> if not res1 then retry else return True
