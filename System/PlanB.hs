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
  , moveByRenaming
  , overrideIfExists
  , useIfExists )
where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class (MonadIO (..))
import Data.Bool (bool)
import Data.Maybe (fromMaybe)
import Path
import System.IO.Error
import System.PlanB.Type
import qualified Path.IO as P

----------------------------------------------------------------------------
-- Operations on files

-- | Create new file. Name of the file is taken as the second argument. The
-- third argument allows to perform actions (in simplest case just creation
-- of file), result of those actions should be new file with given file
-- name.
--
-- This action throws 'alreadyExistsErrorType' by default instead of
-- silently overwriting already existing file, use 'overrideIfExists' and
-- 'useIfExists' to change this behavior.

withNewFile :: (MonadIO m, MonadMask m)
  => PbConfig 'New     -- ^ Configuration
  -> Path b File       -- ^ Name of file to create
  -> (Path Abs File -> m a) -- ^ Given name of temporary file, do it
  -> m a
withNewFile pbc fpath action = withTempDir pbc $ \tdir -> do
  let apath = constructFilePath tdir fpath
  checkExistenceOfFile pbc apath fpath
  liftM2 const (action apath) (moveFile pbc apath fpath)

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
  liftM2 const (action apath) (moveFile pbc apath fpath)

----------------------------------------------------------------------------
-- Operations on directories

-- | Create new directory. Name of the directory is specified as the second
-- argument. The third argument allows to perform actions in “sandboxed”
-- version of new directory.
--
-- This action throws 'alreadyExistsErrorType' by default instead of
-- silently overwriting already existing directory, use 'overrideIfExists'
-- and 'useIfExists'to change this behavior.

withNewDir :: (MonadIO m, MonadMask m)
  => PbConfig 'New     -- ^ Configuration
  -> Path b Dir        -- ^ Name of directory to create
  -> (Path Abs Dir -> m a) -- ^ Given name of temporary directory, do it
  -> m a
withNewDir pbc dpath action = withTempDir pbc $ \tdir -> do
  checkExistenceOfDir pbc tdir dpath
  liftM2 const (action tdir) (moveDir pbc tdir dpath)

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
  liftM2 const (action tdir) (moveDir pbc tdir dpath)

----------------------------------------------------------------------------
-- Operations on containers

-- | Create new container file. This is suitable for processing of all sorts
-- of archive-like objects. The first and second arguments specify how to
-- unpack directory from file and pack it back. The fourth argument names
-- new file. The fifth argument allows to perform actions knowing name of
-- temporary directory.
--
-- This action throws 'alreadyExistsErrorType' by default instead of
-- silently overwriting already existing file, use 'overrideIfExists' and
-- 'useIfExists'to change this behavior.

withNewContainer :: (MonadIO m, MonadMask m)
  => (Path Abs File -> Path Abs Dir -> m ())
     -- ^ How to unpack file into specified directory
  -> (Path Abs Dir -> Path b File -> m ())
     -- ^ How to pack specified directory into file
  -> PbConfig 'New     -- ^ Configuration
  -> Path b File       -- ^ Name of container to create
  -> (Path Abs Dir -> m a) -- ^ Given name of temporary directory, do it
  -> m a
withNewContainer unpack pack pbc fpath action =
  withTempDir pbc $ \tdir -> do
    withTempDir pbc $ \udir -> do
      let apath = constructFilePath udir fpath
      checkExistenceOfFile pbc apath fpath
      using <- P.doesFileExist apath
      when using (unpack apath tdir)
    liftM2 const (action tdir) (pack tdir fpath)

-- | Edit existing container file. This is suitable for processing of all
-- sorts of archive-like objects. The first and second arguments specify how
-- to unpack directory from file and pack it back (overwriting old
-- version). Fourth argument names container file to edit. The last argument
-- allows to perform actions knowing name of temporary directory.
--
-- This action throws 'doesNotExistErrorType' exception if target file does
-- not exist.

withExistingContainer :: (MonadIO m, MonadMask m)
  => (Path b File -> Path Abs Dir -> m ())
     -- ^ How to unpack file into specified directory
  -> (Path Abs Dir -> Path b File -> m ())
     -- ^ How to pack specified directory into file
  -> PbConfig 'Existing -- ^ Configuration
  -> Path b File       -- ^ Name of container to edit
  -> (Path Abs Dir -> m a) -- ^ Given name of temporary directory, do it
  -> m a
withExistingContainer unpack pack pbc fpath action =
  withTempDir pbc $ \tdir -> do
    unpack fpath tdir
    liftM2 const (action tdir) (pack tdir fpath)

----------------------------------------------------------------------------
-- Helpers

-- | Use temporary directory. This action is controlled by supplied
-- configuration, see 'HasTemp'. The temporary directory is removed
-- automatically when given action finishes, although this can be changed
-- via the mentioned configuration value too. If given action finishes
-- successfully, temporary directory is always deleted.

withTempDir :: (HasTemp c, MonadIO m, MonadMask m)
  => c                 -- ^ Configuration
  -> (Path Abs Dir -> m a) -- ^ Action to perform with the temporary file
  -> m a
withTempDir pbc action = bracketOnError make freeOptionally $ \dir ->
  liftM2 const (action dir) (freeAlways dir)
  where
    make = do
      tsys <- P.getTempDir
      let tdir = fromMaybe tsys (getTempDir pbc)
      P.createDirIfMissing True tdir
      let ntmp = fromMaybe "plan-b" (getNameTemplate pbc)
      P.createTempDir tdir ntmp
    freeAlways = ignoringIOErrors . P.removeDirRecur
    freeOptionally = unless (getPreserveCorpse pbc) . freeAlways

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
  exists <- P.doesFileExist fpath
  when exists $
    case howHandleExisting pbc of
      Nothing -> throwM $
        mkIOError alreadyExistsErrorType location Nothing (Just ffile)
      Just AebOverride -> return ()
      Just AebUse -> copyFile fpath apath

-- | Check existence of directory and perform actions according to given
-- configuration. See 'checkExistenceOfFile', overall behavior is the same.

checkExistenceOfDir :: (CanHandleExisting c, MonadIO m, MonadThrow m)
  => c                 -- ^ Configuration
  -> Path Abs Dir      -- ^ Where to copy directory (when we have 'AebUse')
  -> Path b   Dir      -- ^ Directory to check
  -> m ()
checkExistenceOfDir pbc apath dpath = liftIO $ do
  let ddir = toFilePath dpath
      location = "System.PlanB.checkExistenceOfDir"
  exists <- P.doesDirExist dpath
  when exists $
    case howHandleExisting pbc of
      Nothing -> throwM $
        mkIOError alreadyExistsErrorType location Nothing (Just ddir)
      Just AebOverride -> return ()
      Just AebUse -> copyDir dpath apath

-- | Move specified file to another location. File can be moved either by
-- copying or by renaming, exact method is determined by supplied
-- configuration.

moveFile :: (HasTemp c, MonadIO m)
  => c                 -- ^ Configuration
  -> Path b0 File      -- ^ Original location
  -> Path b1 File      -- ^ Where to move
  -> m ()
moveFile pbc = bool P.copyFile P.renameFile (getMoveByRenaming pbc)

-- | Move specified directory to another location. If destination location
-- is already occupied, delete that object first. Directory can be moved
-- either by copying or by renaming, exact method is determined by supplied
-- configuration.

moveDir :: (HasTemp c, MonadIO m, MonadCatch m)
  => c                 -- ^ Configuration
  -> Path b0 Dir       -- ^ Original location
  -> Path b1 Dir       -- ^ Where to move
  -> m ()
moveDir pbc src dest = do
  exists <- P.doesDirExist dest
  when exists (P.removeDirRecur dest)
  bool P.copyDirRecur P.renameDir (getMoveByRenaming pbc) src dest

-- | Copy file to new location. Throw 'doesNotExistErrorType' if it does not
-- exist.

copyFile :: (MonadIO m, MonadThrow m)
  => Path b0 File      -- ^ Original location
  -> Path b1 File      -- ^ Where to put copy of the file
  -> m ()
copyFile src dest = liftIO $ do
  let fsrc = toFilePath src
      location = "System.PlanB.copyFile"
  exists <- P.doesFileExist src
  if exists
    then P.copyFile src dest
    else throwM $
      mkIOError doesNotExistErrorType location Nothing (Just fsrc)

-- | Copy contents of one directory into another (recursively). Source
-- directory must exist, otherwise 'doesNotExistErrorType' is
-- thrown. Destination directory will be created if it doesn't exist.

copyDir :: (MonadIO m, MonadCatch m)
  => Path b0 Dir       -- ^ Original location
  -> Path b1 Dir       -- ^ Where to put copy of the directory
  -> m ()
copyDir src dest = do
  let fsrc = toFilePath src
      location = "System.PlanB.copyDir"
  exists <- P.doesDirExist src
  if exists
     then P.copyDirRecur src dest
     else throwM $
       mkIOError doesNotExistErrorType location Nothing (Just fsrc)

-- | Perform specified action ignoring IO exceptions it may throw.

ignoringIOErrors :: MonadCatch m => m () -> m ()
ignoringIOErrors ioe = ioe `catch` handler
  where
    handler :: MonadThrow m => IOError -> m ()
    handler = const (return ())
