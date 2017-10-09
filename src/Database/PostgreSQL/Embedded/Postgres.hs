module Database.PostgreSQL.Embedded.Postgres
    ( startPostgres
    , stopPostgres
    , checkPostgresStarted
    ) where

import           Control.Concurrent                    (threadDelay)
import           Control.Exception                     (SomeException, try)
import           Data.Conduit.Shell                    (grep, rm, run, shell,
                                                        ($|))
import           Data.Conduit.Shell.Segments           (strings)
import           Data.List                             (isInfixOf)
import           Data.Monoid                           ((<>))
import           System.FilePath.Posix                 ((</>))
import           System.Info                           (os)

import           Database.PostgreSQL.Embedded.Download
import           Database.PostgreSQL.Embedded.Types

startPostgres :: StartupConfig -> DBConfig -> IO RuntimeConfig
startPostgres (StartupConfig version_ t) (DBConfig p u) = do
    e <- downloadPostgres getOS version_
    let d = e </> "data"
    run $ do
        shell $ e </> "bin" </> "initdb" <> " -A trust -U " <> u <> " -D " <> d <> " -E UTF-8"
        shell $ e </> "bin" </> "pg_ctl" <> " -D " <> d <> " -o \"-F -p " <>
            (show p) <> "\"" <> " -l " <> (e </> "log") <> " start"
    let r = RuntimeConfig e d
    checkPostgresStarted r t
    return $ r
    where
        getOS | "darwin" `isInfixOf` os = OSX
              | "linux" `isInfixOf` os = Linux
              | "win" `isInfixOf` os = Win
              | otherwise = error $ "Unsupported platform" <> os

stopPostgres :: RuntimeConfig -> IO ()
stopPostgres (RuntimeConfig e d) = run $ do
        shell $ e </> "bin" </> "pg_ctl" <> " -D " <> d <> " stop"
        rm "-rf" d

checkPostgresStarted :: RuntimeConfig -> Int -> IO ()
checkPostgresStarted config secs = checkPostgresStarted_ config secs >>= \_ -> return ()
    where
        checkPostgresStarted_ :: RuntimeConfig -> Int -> IO Bool
        checkPostgresStarted_ (RuntimeConfig e d) n = do
            let oneSec = 1000000
            let check = run $
                        strings (
                            shell (e </> "bin" </> "pg_ctl" <> " -D " <> d <> " status")
                            $| grep "running") >>= \s -> return $ null s

            res <- try check :: IO (Either SomeException Bool)
            let retry = if n > 0 then threadDelay oneSec >>=
                            \_ -> checkPostgresStarted_ config (n - 1)
                        else
                            return False
            case res of
                Left err   -> print err >>= \_ -> retry
                Right res1 -> if res1 then retry else return True
