{-# LANGUAGE OverloadedStrings #-}
module Control.Monad.NestedResource.Linux
  ( nestedPushDir
  , nestedMount
  , nestedBindMount
  , nestedMakeTmpDir
  ) where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.NestedResource (NestedResource, nestIO, nestIO_)
import System.Directory (createDirectory, doesDirectoryExist, getCurrentDirectory, removeDirectory)
import System.Posix.Directory (changeWorkingDirectory)
import System.Linux.Mount


nestedPushDir :: FilePath -> NestedResource ()
nestedPushDir newDir =
  nestIO_ changeDir changeWorkingDirectory
  where
    changeDir = do
      current <- getCurrentDirectory
      changeWorkingDirectory newDir
      return current


nestedMount :: String -> FilePath -> String -> [MountFlag] -> NestedResource ()
nestedMount device mountPoint fsType opts =
  nestIO mountDevice (const $ umountWith Detach Follow mountPoint)
  where
    mountDevice = mount device mountPoint fsType opts ""


nestedBindMount :: String -> FilePath -> NestedResource ()
nestedBindMount source target =
  nestIO (bind source target) (const $ umount target)


nestedMakeTmpDir :: FilePath -> NestedResource ()
nestedMakeTmpDir path = do
  -- Should fail and catch exception.
  exists <- liftIO $ doesDirectoryExist path
  unless exists $
    nestIO_ (createDirectory path) (const $ removeDirectory path)
