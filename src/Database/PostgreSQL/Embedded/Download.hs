module Database.PostgreSQL.Embedded.Download
    ( downloadPostgres
    ) where

import           System.Process                     (rawSystem)
import           Data.Monoid                        ((<>))
import           System.Directory                   (setCurrentDirectory,
                                                     createDirectoryIfMissing,
                                                     doesDirectoryExist,
                                                     getHomeDirectory)
import           System.FilePath.Posix              ((</>))

import           Database.PostgreSQL.Embedded.Types
import           Database.PostgreSQL.Embedded.Exec

data ArchiveType = Zip | Tar deriving (Eq)

downloadPostgres :: Bool -> Os -> Version -> IO FilePath
downloadPostgres silent_ os_ version_ = do
    home <- getHomeDirectory

    let v = value version_
    let workdir = home </> ".postgres-embedded" </> v
    let tmp = workdir </> "postgres.tmp"
    let dist = workdir </> "pgsql"
    let execSystem = if silent_ then rawSystemSilent else rawSystem

    createDirectoryIfMissing True workdir

    exists <- doesDirectoryExist dist
    case exists of
        True -> return dist
        False -> do
            setCurrentDirectory workdir
            let (aType, suffix) = binaries os_
            _ <- execSystem "wget" ["-O", tmp, base_download_url <> v <> suffix]
            _ <- case aType of
                Zip -> execSystem "unzip" ["-q", tmp]
                Tar -> execSystem "tar" ["-xzf", tmp]
            return dist

            where
                binaries :: Os -> (ArchiveType, String)
                binaries OSX   = (Zip, "-osx-binaries.zip")
                binaries Win   = (Zip, "-windows-x64-binaries.zip")
                binaries Linux = (Tar, "-linux-x64-binaries.tar.gz")

                base_download_url = "http://get.enterprisedb.com/postgresql/postgresql-"
