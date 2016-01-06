-- |
-- Module      :  System.PlanB
-- Copyright   :  © 2016 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Failure-tolerant file and directory editing. All functions here can
-- recover from exceptions thrown while they work. They bring file-system to
-- the state it was in before specific function was called. Temporary files
-- and backups are handled automatically.

{-# LANGUAGE DataKinds #-}

module System.PlanB
  ( -- * Operations on files
    withNewFile
  , withExistingFile
    -- * Operations on directories
  , withNewDir
  , withExistingDir
    -- * Operations on containers
  , withNewContainer
  , withExistingContainer
    -- * Configuration options
  , tempDir
  , nameTemplate
  , preserveCorpse
  , overrideIfExists
  , useIfExists )
where

import Control.Monad (when, unless, liftM2)
import Control.Monad.Catch
import Control.Monad.IO.Class (MonadIO (..))
import Data.Maybe (fromMaybe)
import Path
import System.IO.Error
import System.IO.Temp
import System.PlanB.Type
import qualified System.Directory as Dir

-- | Create new file. Name of the file is taken as the second argument. The
-- third argument allows to perform actions (in simplest case just creation
-- of file), result of those actions should be new file with given file
-- name.
--
-- This action throws 'alreadyExistsErrorType' by default instead of
-- silently overwriting already existing file, use 'overrideIfExists' and
-- 'useIfExists'to change this behavior.

withNewFile :: (MonadIO m, MonadMask m)
  => PbConfig 'New     -- ^ Configuration
  -> Path b File       -- ^ Name of file to create
  -> (Path Abs File -> m a) -- ^ Given name of temporary file, do it
  -> m a
withNewFile pbc fpath action = withTempDir pbc $ \tdir -> do
  let apath = constructFilePath tdir fpath
  checkExistenceOfFile pbc apath fpath
  liftM2 const (action apath) (moveFile apath fpath)

-- | Edit existing file. Name of the file is taken as the second
-- argument. The third argument allows to perform actions on temporary copy
-- of specified file.
--
-- This action throws 'doesNotExistErrorType' exception if target file does
-- not exist.

withExistingFile :: (MonadIO m, MonadMask m)
  => PbConfig 'Existing -- ^ Configuration
  -> Path b File       -- ^ Name of file to edit
  -> (Path Abs File -> m a) -- ^ Given name of temporary file, do it
  -> m a
withExistingFile pbc fpath action = withTempDir pbc $ \tdir -> do
  let apath = constructFilePath tdir fpath
  copyFile fpath apath
  liftM2 const (action apath) (moveFile apath fpath)

-- | Create new directory. Name of the directory is specified as the second
-- argument. The third argument allows to perform actions in “sandboxed”
-- version of new directory.
--
-- This action throws 'alreadyExistsErrorType' by default instead of
-- silently overwriting already existing directory, see 'overrideIfExists'
-- and 'useIfExists'to change this behavior.

withNewDir :: (MonadIO m, MonadMask m)
  => PbConfig 'New     -- ^ Configuration
  -> Path b Dir        -- ^ Name of directory to create
  -> (Path Abs Dir -> m a) -- ^ Given name of temporary directory, do it
  -> m a
withNewDir pbc dpath action = withTempDir pbc $ \tdir -> do
  checkExistenceOfDir pbc tdir dpath
  liftM2 const (action tdir) (moveDir tdir dpath)

-- | Edit existing directory. Name of the directory is specified as the
-- second argument. The third argument allows to perform actions in
-- “sandboxed” copy of target directory.
--
-- This action throws 'doesNotExistErrorType' exception if target directory
-- does not exist.

withExistingDir :: (MonadIO m, MonadMask m)
  => PbConfig 'Existing -- ^ Configuration
  -> Path b Dir        -- ^ Name of directory to edit
  -> (Path Abs Dir -> m a) -- ^ Given name of temporary directory, do it
  -> m a
withExistingDir pbc dpath action = withTempDir pbc $ \tdir -> do
  copyDir dpath tdir
  liftM2 const (action tdir) (moveDir tdir dpath)

-- | Create new container file. This is suitable for processing of all sorts
-- of archive-like objects. The first argument tells how to pack directory
-- into a file. The third argument names new file. The fourth argument
-- allows to perform actions knowing name of temporary directory.
--
-- This action throws 'alreadyExistsErrorType' by default instead of
-- silently overwriting already existing file, see 'overrideIfExists' and
-- 'useIfExists'to change this behavior.

withNewContainer :: (MonadIO m, MonadMask m)
  => (Path b File -> Path b Dir -> m ())
     -- ^ How to unpack file into specified directory
  -> (Path b Dir -> Path b File -> m ())
     -- ^ How to pack specified directory into file
  -> PbConfig 'New     -- ^ Configuration
  -> Path b File       -- ^ Name of container to create
  -> (Path Abs Dir -> m a) -- ^ Given name of temporary directory, do it
  -> m a
withNewContainer unpack pack pbc fpath action = undefined
-- ↑ TODO packing is easy, where to put unpacking?

-- | Edit existing container file. This is suitable for processing of all
-- sorts of archive-like objects. The first and second arguments specify how
-- to unpack directory from file and pack it back. Fourth argument names
-- container file to edit. The last argument allows to perform actions
-- knowing name of temporary directory.
--
-- This action throws 'doesNotExistErrorType' exception if target file does
-- not exist.

withExistingContainer :: (MonadIO m, MonadMask m)
  => (Path b File -> Path b Dir -> m ())
     -- ^ How to unpack file into specified directory
  -> (Path b Dir -> Path b File -> m ())
     -- ^ How to pack specified directory into file
  -> PbConfig 'Existing -- ^ Configuration
  -> Path b File       -- ^ Name of container to edit
  -> (Path Abs Dir -> m a) -- ^ Given name of temporary directory, do it
  -> m a
withExistingContainer unpack pack pbc fpath action = undefined
-- ↑ TODO should be easy

----------------------------------------------------------------------------
-- Helpers

-- | Use temporary directory. This action is controlled by supplied
-- configuration, see 'HasTemp'. The temporary directory is removed
-- automatically when given action finished, although this can be changed
-- via the mentioned configuration value too.

withTempDir :: (HasTemp c, MonadIO m, MonadMask m)
  => c                 -- ^ Configuration
  -> (Path Abs Dir -> m a) -- ^ Action to perform with the temporary file
  -> m a
withTempDir pbc = bracket make free
  where
    make = liftIO $ do
      tsys <- Dir.getTemporaryDirectory
      let tdir = fromMaybe tsys (getTempDir pbc)
      Dir.createDirectoryIfMissing True tdir
      let ntmp = fromMaybe "plan-b" (getNameTemplate pbc)
      createTempDirectory tdir ntmp >>= parseAbsDir
    free = liftIO
      . unless (getPreserveCorpse pbc)
      . ignoringIOErrors
      . Dir.removeDirectoryRecursive
      . fromAbsDir

-- | Construct name of file combining given directory path and file name
-- from path to file.

constructFilePath
  :: Path Abs Dir      -- ^ Directory name
  -> Path b File       -- ^ Get file name from this path
  -> Path Abs File     -- ^ Resulting path
constructFilePath dir file = dir </> filename file

-- | Check existence of file and perform actions according to given
-- configuration. By default we throw 'alreadyExistsErrorType' unless user
-- has specified different 'AlreadyExistsBehavior'. If it's 'AebOverride',
-- then we don't need to do anything, file will be overwritten
-- automatically, if we have 'AebUse', then we should copy it into given
-- directory.

checkExistenceOfFile :: (CanHandleExisting c, MonadIO m, MonadThrow m)
  => c                 -- ^ Configuration
  -> Path Abs File     -- ^ Where to copy file (when we have 'AebUse')
  -> Path b   File     -- ^ File to check
  -> m ()
checkExistenceOfFile pbc apath fpath = liftIO $ do
  let ffile = toFilePath fpath
      location = "System.PlanB.checkExistenceOfFile"
  exists <- Dir.doesFileExist ffile
  when exists $
    case howHandleExisting pbc of
      Nothing -> throwM $
        mkIOError alreadyExistsErrorType location Nothing (Just ffile)
      Just AebOverride -> return ()
      Just AebUse -> copyFile fpath apath

checkExistenceOfDir :: (CanHandleExisting c, MonadIO m, MonadThrow m)
  => c                 -- ^ Configuration
  -> Path Abs Dir      -- ^ Where to copy directory (when we have 'AebUse')
  -> Path b   Dir      -- ^ Directory to check
  -> m ()
checkExistenceOfDir pbc apath dpath = liftIO $ do
  let ddir = toFilePath dpath
      location = "System.PlanB.checkExistenceOfDir"
  exists <- Dir.doesDirectoryExist ddir
  when exists undefined -- TODO
    -- case howHandleExisting pbc of
    --   Nothing -> throwM $
    --     mkIOError alreadyExistErrorType location Nothing (Just ddir)
    --   Just AebOverride ->

-- | Move file to new location silently replacing file there, if it already
-- exists.

moveFile :: MonadIO m
  => Path b0 File      -- ^ Original location
  -> Path b1 File      -- ^ Where to move
  -> m ()
moveFile base dest = liftIO $
  Dir.renameFile (toFilePath base) (toFilePath dest)

-- | FIXME Describe

moveDir :: MonadIO m
  => Path b0 Dir       -- ^ Original location
  -> Path b1 Dir       -- ^ Where to move
  -> m ()
moveDir base dest = undefined -- TODO write, overwrite existing too

-- | Copy file to new location. Throw 'doesNotExistErrorType' is it does not
-- exist.

copyFile :: (MonadIO m, MonadThrow m)
  => Path b0 File      -- ^ Original location
  -> Path b1 File      -- ^ Where to put copy of the file
  -> m ()
copyFile base dest = liftIO $ do
  let fbase = toFilePath base
      location = "System.PlanB.copyFile"
  exists <- Dir.doesFileExist fbase
  if exists
    then Dir.copyFile fbase (toFilePath dest)
    else throwM $
      mkIOError doesNotExistErrorType location Nothing (Just fbase)

-- | FIXME Describe

copyDir :: (MonadIO m, MonadThrow m)
  => Path b0 Dir       -- ^ Original location
  -> Path b1 Dir       -- ^ Where to put copy of the directory
  -> m ()
copyDir base dest = undefined -- TODO write, throw if does not exists

-- | Perform specified action ignoring IO exceptions it may throw.

ignoringIOErrors :: IO () -> IO ()
ignoringIOErrors ioe = ioe `catch` handler
  where
    handler :: IOError -> IO ()
    handler = const (return ())
