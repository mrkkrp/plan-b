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

import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.IO.Class (MonadIO (..))
import Path
import System.Directory
import System.IO.Error
import System.PlanB.Type

-- | Create new file. Name of the file is taken as the second argument. The
-- third argument allows to perform actions (in simplest case just creation
-- of file), result of those actions should be new file with given file
-- name.
--
-- This action throws 'alreadyExistsErrorType' by default instead of
-- silently overwriting already existing file, see 'overrideIfExists' and
-- 'useIfExists'to change this behavior.

withNewFile :: (MonadIO m, MonadThrow m)
  => PbConfig 'New     -- ^ Configuration
  -> Path b File       -- ^ Name of file to create
  -> (Path Abs File -> m a) -- ^ Given name of temporary file, do it
  -> m a
withNewFile = undefined

-- | Edit existing file. Name of the file is taken as the second
-- argument. The third argument allows to perform actions on temporary copy
-- of specified file.
--
-- This action throws 'doesNotExistErrorType' exception if target file does
-- not exist.

withExistingFile :: (MonadIO m, MonadThrow m)
  => PbConfig 'Existing -- ^ Configuration
  -> Path b File       -- ^ Name of file to edit
  -> (Path Abs File -> m a) -- ^ Given name of temporary file, do it
  -> m a
withExistingFile = undefined

-- | Create new directory. Name of the directory is specified as the second
-- argument. The third argument allows to perform actions in “sandboxed”
-- version of new directory.
--
-- This action throws 'alreadyExistsErrorType' by default instead of
-- silently overwriting already existing directory, see 'overrideIfExists'
-- and 'useIfExists'to change this behavior.

withNewDir :: (MonadIO m, MonadThrow m)
  => PbConfig 'New     -- ^ Configuration
  -> Path b Dir        -- ^ Name of directory to create
  -> (Path Abs Dir -> m a) -- ^ Given name of temporary directory, do it
  -> m a
withNewDir = undefined

-- | Edit existing directory. Name of the directory is specified as the
-- second argument. The third argument allows to perform actions in
-- “sandboxed” copy of target directory.
--
-- This action throws 'doesNotExistErrorType' exception if target directory
-- does not exist.

withExistingDir :: (MonadIO m, MonadThrow m)
  => PbConfig 'Existing -- ^ Configuration
  -> Path b Dir        -- ^ Name of directory to edit
  -> (Path Abs Dir -> m a) -- ^ Given name of temporary directory, do it
  -> m a
withExistingDir = undefined

-- | Create new container file. This is suitable for processing of all sorts
-- of archive-like objects. The first argument tells how to pack directory
-- into a file. The third argument names new file. The fourth argument
-- allows to perform actions knowing name of temporary directory.
--
-- This action throws 'alreadyExistsErrorType' by default instead of
-- silently overwriting already existing file, see 'overrideIfExists' and
-- 'useIfExists'to change this behavior.

withNewContainer :: (MonadIO m, MonadThrow m)
  => (Path b Dir -> Path b File -> m ())
     -- ^ How to pack specified directory into file
  -> PbConfig 'New     -- ^ Configuration
  -> Path b File       -- ^ Name of container to create
  -> (Path Abs Dir -> m a) -- ^ Given name of temporary directory, do it
  -> m a
withNewContainer = undefined

-- | Edit existing container file. This is suitable for processing of all
-- sorts of archive-like objects. The first and second arguments specify how
-- to unpack directory from file and pack it back. Fourth argument names
-- container file to edit. The last argument allows to perform actions
-- knowing name of temporary directory.
--
-- This action throws 'doesNotExistErrorType' exception if target file does
-- not exist.

withExistingContainer :: (MonadIO m, MonadThrow m)
  => (Path b File -> Path b Dir -> m ())
     -- ^ How to unpack file into specified directory
  -> (Path b Dir -> Path b File -> m ())
     -- ^ How to pack specified directory into file
  -> PbConfig 'Existing -- ^ Configuration
  -> Path b File       -- ^ Name of container to edit
  -> (Path Abs Dir -> m a) -- ^ Given name of temporary directory, do it
  -> m a
withExistingContainer = undefined
