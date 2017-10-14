module Database.PostgreSQL.Embedded.Postgres
    ( startPostgres
    , stopPostgres
    , checkPostgresStarted
    ) where

import           Control.Exception                     (SomeException, try)
import           Data.Conduit.Shell                    (rm, run, shell)
import           Data.List                             (isInfixOf)
import           Data.Monoid                           ((<>))
import           System.Directory                      (doesPathExist)
import           System.Exit                           (exitFailure)
import           System.FilePath.Posix                 ((</>))
import           System.Info                           (os)

import           Database.PostgreSQL.Embedded.Download
import           Database.PostgreSQL.Embedded.Types

startPostgres :: StartupConfig -> DBConfig -> IO RuntimeConfig
startPostgres (StartupConfig сlean version_ t) dConfig@(DBConfig p u) = do
    e <- downloadPostgres getOS version_
    let d = e </> "data"
    exists <- doesPathExist d
    if exists && not сlean then do
        putStrLn $ "Directory " <> d <> " must not exist"
        exitFailure
    else run $ rm "-rf" d

    run $ do
        shell $ e </> "bin" </> "initdb" <> " -A trust -U " <> u <> " -D " <> d <> " -E UTF-8"
        shell $ e </> "bin" </> "pg_ctl" <> " -D " <> d <> " -o \"-F -p " <>
            (show p) <> "\"" <> " -l " <> (e </> "log") <> " start"

    let r = RuntimeConfig e d
    checkPostgresStarted r dConfig t
    return $ r

    where
        getOS | "darwin" `isInfixOf` os = OSX
              | "linux" `isInfixOf` os = Linux
              | "win" `isInfixOf` os = Win
              | otherwise = error $ "Unsupported platform" <> os

stopPostgres :: RuntimeConfig -> IO ()
stopPostgres (RuntimeConfig e d) = run $
        shell $ e </> "bin" </> "pg_ctl" <> " -D " <> d <> " stop" <>
                " -m fast -t 5 -w"

checkPostgresStarted :: RuntimeConfig -> DBConfig -> Int -> IO ()
checkPostgresStarted (RuntimeConfig e _) (DBConfig p _) secs = checkPostgresStarted_ secs >>= \_ -> return ()
    where
        checkPostgresStarted_ :: Int -> IO Bool
        checkPostgresStarted_ n = do
            let check = run $ shell (e </> "bin" </> "pg_isready" <> " -p " <> (show p) <> " -h localhost") >>= \_ -> return True

            res <- try check :: IO (Either SomeException Bool)
            let retry = if n > 0 then checkPostgresStarted_ (n - 1)
                        else return False
            case res of
                Left err   -> print err >>= \_ -> retry
                Right res1 -> if (not res1) then retry else return True
