module Database.PostgreSQL.Embedded.Download
    ( downloadPostgres
    ) where

import           Data.Conduit.Shell                 (cd, run, tar, unzip', wget)
import           Data.Monoid                        ((<>))
import           System.Directory                   (createDirectoryIfMissing,
                                                     doesDirectoryExist,
                                                     getHomeDirectory)
import           System.FilePath.Posix              ((</>))

import           Database.PostgreSQL.Embedded.Types

data ArchiveType = Zip | Tar deriving (Eq)

downloadPostgres :: Os -> Version -> IO (FilePath)
downloadPostgres os_ version_ = do
    home <- getHomeDirectory

    let v = value version_
    let workdir = home </> ".postgres-embedded" </> v
    let tmp = workdir </> "postgres.tmp"
    let dist = workdir </> "pgsql"

    createDirectoryIfMissing True workdir

    exists <- doesDirectoryExist dist
    case exists of
        True -> return dist
        False -> run $ do
            cd workdir

            let (aType, suffix) = binaries os_

            wget "-O" tmp $ base_download_url <> v <> suffix

            case aType of
                Zip -> unzip' tmp
                Tar -> tar "-xzvf" tmp

            return dist

            where
                binaries :: Os -> (ArchiveType, String)
                binaries OSX   = (Zip, "-osx-binaries.zip")
                binaries Win   = (Zip, "-windows-x64-binaries.zip")
                binaries Linux = (Tar, "-linux-x64-binaries.tar.gz")

                base_download_url = "http://get.enterprisedb.com/postgresql/postgresql-"
