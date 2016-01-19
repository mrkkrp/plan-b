-- |
-- Module      :  System.PlanB.Type
-- Copyright   :  © 2016 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Types and type classes for “Plan B” library. You usually don't need to
-- import this module because "System.PlanB" already exports everything you
-- need.

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}

module System.PlanB.Type
  ( Subject (..)
  , AlreadyExistsBehavior (..)
  , PbConfig
  , HasTemp (..)
  , CanHandleExisting (..) )
where

import Control.Applicative
import Data.Monoid ((<>), Any (..))
import Path

-- | We use this as named kind with two promoted constructors. The
-- constructors are used as phantom types to index 'PbConfig'.

data Subject = New | Existing

-- | Custom behavior for cases when something already exists.

data AlreadyExistsBehavior
  = AebOverride        -- ^ First delete that object, then do your thing
  | AebUse             -- ^ Continue to work with that object instead
  deriving (Eq, Enum, Bounded)

-- | The configuration allows to control behavior of the library in
-- details. It's a 'Monoid' and so various configuration settings can be
-- combined using 'mappend' while 'mempty' represents default behavior.
--
-- When combining conflicting configuration settings, the value on the left
-- side of 'mappend' wins:
--
-- > overrideIfExists <> useIfExists -- will override

data PbConfig :: Subject -> * where
  PbConfig ::
    { pbcTempDir        :: Maybe (Path Abs Dir)
    , pbcNameTemplate   :: Maybe FilePath
    , pbcPreserveCorpse :: Any
    , pbcAlreadyExists  :: Maybe AlreadyExistsBehavior
    } -> PbConfig k

instance Monoid (PbConfig k) where
  mempty  = PbConfig empty empty mempty empty
  x `mappend` y = PbConfig
    { pbcTempDir        = pbcTempDir x       <|> pbcTempDir y
    , pbcNameTemplate   = pbcNameTemplate x  <|> pbcNameTemplate y
    , pbcPreserveCorpse = pbcPreserveCorpse x <> pbcPreserveCorpse y
    , pbcAlreadyExists  = pbcAlreadyExists x <|> pbcAlreadyExists y }

-- | The type class is for data types that include information specifying
-- how to create temporary files and directories and whether to delete them
-- automatically or not.

class HasTemp c where

  -- | Specifies name of temporary directory to use. Default is the system
  -- temporary directory. If the directory does not exist, it will be
  -- created, but not deleted.

  tempDir :: Path Abs Dir -> c

  -- | Specify template to use to name temporary directory, see
  -- 'System.Directory.openTempFile', default is @\"plan-b\"@.

  nameTemplate :: FilePath -> c

  -- | 'preserveCorpse' preserves temporary files and directories when
  -- exception is thrown (normally they are removed).

  preserveCorpse :: c

  getTempDir        :: c -> Maybe (Path Abs Dir)
  getNameTemplate   :: c -> Maybe FilePath
  getPreserveCorpse :: c -> Bool

instance HasTemp (PbConfig k) where

  tempDir dir       = mempty { pbcTempDir        = Just dir }
  nameTemplate nt   = mempty { pbcNameTemplate   = Just nt  }
  preserveCorpse    = mempty { pbcPreserveCorpse = Any True }

  getTempDir        = pbcTempDir
  getNameTemplate   = pbcNameTemplate
  getPreserveCorpse = getAny . pbcPreserveCorpse

-- | The type class includes data types that contain information about what
-- to do when some object already exists. There are two scenarios currently
-- supported: override it or use it.

class CanHandleExisting c where

  -- | The option allows to avoid throwing exception if upon completion of
  -- specified action file with given name already exists in the file
  -- system.

  overrideIfExists :: c

  -- | The option will copy already existing file into temporary location,
  -- so you can edit it instead of creating new file.

  useIfExists :: c

  howHandleExisting :: c -> Maybe AlreadyExistsBehavior

instance CanHandleExisting (PbConfig 'New) where

  overrideIfExists  = mempty { pbcAlreadyExists = Just AebOverride }
  useIfExists       = mempty { pbcAlreadyExists = Just AebUse      }

  howHandleExisting = pbcAlreadyExists
