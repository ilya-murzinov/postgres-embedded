module Database.PostgreSQL.Embedded.Exec
  ( systemSilent
  , rawSystemSilent
  ) where

import           GHC.IO.Exception (IOErrorType (..), ioException)
import           System.Exit
import           System.IO
import           System.IO.Error
import           System.Process

{-|
Exactly like 'System.Process.system' but does not capture stdout nor stderr.
-}
systemSilent :: String -> IO ExitCode
systemSilent "" = ioException
  (ioeSetErrorString (mkIOError InvalidArgument "system" Nothing Nothing) "null command")
systemSilent str = do
  devNull <- openFile "/dev/null" WriteMode
  (_, _, _, p) <- createProcess (shell str)
                  { delegate_ctlc = True
                  , std_err = UseHandle devNull
                  , std_out = UseHandle devNull
                  }
  waitForProcess p

{-|
Exactly like 'System.Process.rawSystem' but does not capture stdout nor stderr.
-}
rawSystemSilent :: String -> [String] -> IO ExitCode
rawSystemSilent cmd args = do
  devNull <- nullDevice
  (_, _, _, p) <- createProcess (proc cmd args)
                  { delegate_ctlc = True
                  , std_err = UseHandle devNull
                  , std_out = UseHandle devNull
                  }
  waitForProcess p

nullDevice :: IO Handle
nullDevice = do
  h <- tryIOError nix
  either (const windows) return h
  where
    nix = openFile "/dev/null" WriteMode
    windows = openFile "nul" WriteMode
