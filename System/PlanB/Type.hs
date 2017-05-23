-- |
-- Module      :  System.PlanB.Type
-- Copyright   :  © 2016–2017 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Types and type classes. You usually don't need to import this module
-- because "System.PlanB" already exports everything you need.

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
import Data.Monoid
import Path

-- | We use this as a kind with two promoted constructors. The constructors
-- are used as phantom types to index 'PbConfig'.

data Subject = New | Existing

-- | Custom behavior for cases when something already exists.

data AlreadyExistsBehavior
  = AebOverride        -- ^ First delete that object, then do your thing
  | AebUse             -- ^ Continue to work with that object instead
  deriving (Eq, Enum, Bounded)

-- | The configuration allows to control behavior of the library in details.
-- It's a 'Monoid' and so various configuration settings can be combined
-- using 'mappend' while 'mempty' represents default behavior.
--
-- When combining conflicting configuration settings, the value on the left
-- side of 'mappend' wins:
--
-- > overrideIfExists <> useIfExists -- will override

data PbConfig :: Subject -> * where
  PbConfig ::
    { pbcTempDir        :: Maybe (Path Abs Dir)
    , pbcNameTemplate   :: Maybe String
    , pbcPreserveCorpse :: Any
    , pbcMoveByRenaming :: Any
    , pbcAlreadyExists  :: Maybe AlreadyExistsBehavior
    } -> PbConfig k

instance Monoid (PbConfig k) where
  mempty  = PbConfig empty empty mempty mempty empty
  x `mappend` y = PbConfig
    { pbcTempDir        = pbcTempDir x       <|> pbcTempDir y
    , pbcNameTemplate   = pbcNameTemplate x  <|> pbcNameTemplate y
    , pbcPreserveCorpse = pbcPreserveCorpse x <> pbcPreserveCorpse y
    , pbcMoveByRenaming = pbcMoveByRenaming x <> pbcMoveByRenaming y
    , pbcAlreadyExists  = pbcAlreadyExists x <|> pbcAlreadyExists y }

-- | The type class is for the data types that include information
-- specifying how to create temporary files and directories and whether to
-- delete them automatically or not.

class HasTemp c where

  -- | Specify name of temporary directory to use. The default is the system
  -- temporary directory. If the directory does not exist, it will be
  -- created, but won't be deleted.

  tempDir :: Path Abs Dir -> c

  -- | Specify the template string to use to name temporary directory, see
  -- 'System.Directory.openTempFile', default is @\"plan-b\"@.

  nameTemplate :: String -> c

  -- | 'preserveCorpse' preserves temporary files and directories when
  -- exception is thrown (normally they are removed).

  preserveCorpse :: c

  -- | By default files are moved by copying. Moving by renaming, although
  -- not always possible, often more efficient. This option enables it.

  moveByRenaming :: c

  getTempDir        :: c -> Maybe (Path Abs Dir)
  getNameTemplate   :: c -> Maybe String
  getPreserveCorpse :: c -> Bool
  getMoveByRenaming :: c -> Bool

instance HasTemp (PbConfig k) where

  tempDir dir       = mempty { pbcTempDir        = Just dir }
  nameTemplate nt   = mempty { pbcNameTemplate   = Just nt  }
  preserveCorpse    = mempty { pbcPreserveCorpse = Any True }
  moveByRenaming    = mempty { pbcMoveByRenaming = Any True }

  getTempDir        = pbcTempDir
  getNameTemplate   = pbcNameTemplate
  getPreserveCorpse = getAny . pbcPreserveCorpse
  getMoveByRenaming = getAny . pbcMoveByRenaming

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
