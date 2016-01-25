--
-- Plan B tests.
--
-- Copyright © 2016 Mark Karpov
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are
-- met:
--
-- * Redistributions of source code must retain the above copyright notice,
--   this list of conditions and the following disclaimer.
--
-- * Redistributions in binary form must reproduce the above copyright
--   notice, this list of conditions and the following disclaimer in the
--   documentation and/or other materials provided with the distribution.
--
-- * Neither the name Mark Karpov nor the names of contributors may be used
--   to endorse or promote products derived from this software without
--   specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS “AS IS” AND ANY
-- EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY
-- DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
-- STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
-- ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.

{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Exception
import Control.Monad
import Data.List ((\\))
import Data.Monoid
import Data.Typeable (Typeable)
import Path
import System.IO.Error
import System.PlanB
import Test.Hspec
import qualified Path.IO as P

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (<$))
#endif

main :: IO ()
main = hspec . around withSandbox $ do
  describe "withNewFile"           withNewFileSpec
  describe "withExistingFile"      withExistingFileSpec
  describe "withNewDir"            withNewDirSpec
  describe "withExistingDir"       withExistingDirSpec
  describe "withNewContainer"      withNewContainerSpec
  describe "withExistingConcainer" withExistingContainerSpec

----------------------------------------------------------------------------
-- Operations on files

withNewFileSpec :: SpecWith (Path Abs Dir)
withNewFileSpec = do

  context "when such file already exists" . beforeWith populatedFile $ do
    context "when we want to error" $
      it "throws the right exception" $ \file -> do
        withNewFile' mempty file pncFile `shouldThrow` isAlreadyExistsError
        detectFile file (Just oldFileCont)
    context "when we want to override it" $
      it "overrides it" $ \file -> do
        withNewFile' overrideIfExists file pncFile
        detectFile file (Just newFileCont)
    context "when we want to use it" $
      it "makes it available for editing" $ \file -> do
        withNewFile' useIfExists file getFileCont `shouldReturn` oldFileCont
        detectFile file (Just oldFileCont)

  context "when such file does not exist" . beforeWith missingFile $ do
    context "when we throw" $ do
      context "when we don't want corpse" $
        it "propagates exception, the corpse is removed" $ \file -> do
          withNewFile' mempty file (pncFile >=> throwExc)
            `shouldThrow` isExc
          detectFile file Nothing
          detectFileCorpse file False
      context "when we want corpse" $
        it "propagates exception, the corpse is there" $ \file -> do
          withNewFile' preserveCorpse file (pncFile >=> throwExc)
            `shouldThrow` isExc
          detectFile file Nothing
          detectFileCorpse file True
    context "when we finish successfully" $
      it "creates the right file, corpse is always removed" $ \file -> do
        withNewFile' preserveCorpse file pncFile
        detectFile file (Just newFileCont)
        detectFileCorpse file False

  where
    withNewFile' pbc p = withNewFile (tempDir (parent p) <> pbc) p

withExistingFileSpec :: SpecWith (Path Abs Dir)
withExistingFileSpec = do

  context "when target file exists" . beforeWith populatedFile $ do
    context "when we throw" $ do
      context "when we don't want corpse" $
        it "propagates exception, the corpse is removed" $ \file -> do
          withExistingFile' mempty file (pncFile >=> throwExc)
            `shouldThrow` isExc
          detectFile file (Just oldFileCont)
          detectFileCorpse file False
      context "when we want corpse" $
        it "propagates exception, the corpse is there" $ \file -> do
          withExistingFile' preserveCorpse file (pncFile >=> throwExc)
            `shouldThrow` isExc
          detectFile file (Just oldFileCont)
          detectFileCorpse file True
    context "when we finish successfully" $
      it "updates the right file, corpse is always removed" $ \file -> do
        withExistingFile' preserveCorpse file pncFile
        detectFile file (Just newFileCont)
        detectFileCorpse file False

  context "when target file is missing" . beforeWith missingFile $
    it "throws the right exception" $ \file -> do
      withExistingFile' mempty file pncFile `shouldThrow` isDoesNotExistError
      detectFile file Nothing

  where
    withExistingFile' pbc p = withExistingFile (tempDir (parent p) <> pbc) p

----------------------------------------------------------------------------
-- Operations on directories

withNewDirSpec :: SpecWith (Path Abs Dir)
withNewDirSpec = do

  context "when such directory already exists" . beforeWith populatedDir $ do
    context "when we want to error" $
      it "throws the right exception" $ \dir -> do
        withNewDir' mempty dir pncDir `shouldThrow` isAlreadyExistsError
        detectDir dir (Just oldDirCont)
    context "when we want to override it" $
      it "overrides it" $ \dir -> do
        withNewDir' overrideIfExists dir pncDir
        detectDir dir (Just newDirCont)
    context "when we want to use it" $
      it "makes it available for editing" $ \dir -> do
        withNewDir' useIfExists dir getDirCont `shouldReturn` oldDirCont
        detectDir dir (Just oldDirCont)

  context "when such directory does not exist" . beforeWith missingDir $ do
    context "when we throw" $ do
      context "when we don't want corpse" $
        it "propagates exception, the corpse is removed" $ \dir -> do
          withNewDir' mempty dir (pncDir >=> throwExc)
            `shouldThrow` isExc
          detectDir dir Nothing
          detectDirCorpse dir False
      context "when we want corpse" $
        it "propagates exception, the corpse is there" $ \dir -> do
          withNewDir' preserveCorpse dir (pncDir >=> throwExc)
            `shouldThrow` isExc
          detectDir dir Nothing
          detectDirCorpse dir True
    context "when we finish successfully" $
      it "creates the right directory, corpse is always removed" $ \dir -> do
        withNewDir' preserveCorpse dir pncDir
        detectDir dir (Just newDirCont)
        detectDirCorpse dir False

  where
    withNewDir' pbc p = withNewDir (tempDir (parent p) <> pbc) p

withExistingDirSpec :: SpecWith (Path Abs Dir)
withExistingDirSpec = do

  context "when target directory exists" . beforeWith populatedDir $ do
    context "when we throw" $ do
      context "when we don't want corpse" $
        it "propagates exception, the corpse is removed" $ \dir -> do
          withExistingDir' mempty dir (pncDir >=> throwExc)
            `shouldThrow` isExc
          detectDir dir (Just oldDirCont)
          detectDirCorpse dir False
      context "when we want corpse" $
        it "propagates exception, the corpse is there" $ \dir -> do
          withExistingDir' preserveCorpse dir (pncDir >=> throwExc)
            `shouldThrow` isExc
          detectDir dir (Just oldDirCont)
          detectDirCorpse dir True
    context "when we finish successfully" $
      it "updates the right directory, corpse is always removed" $ \dir -> do
        withExistingDir' preserveCorpse dir pncDir
        detectDir dir (Just newDirCont)
        detectDirCorpse dir False

  context "when target directory is missing" . beforeWith missingDir $
    it "throws the right exception" $ \dir -> do
      withExistingDir' mempty dir pncDir `shouldThrow` isDoesNotExistError
      detectDir dir Nothing

  where
    withExistingDir' pbc p = withExistingDir (tempDir (parent p) <> pbc) p

----------------------------------------------------------------------------
-- Operations on containers

withNewContainerSpec :: SpecWith (Path Abs Dir)
withNewContainerSpec = do

  context "when container already exists" . beforeWith populatedFile $ do
    context "when we want to error" $
      it "throws the right exception" $ \file -> do
        withNewContainer' mempty file pncDir `shouldThrow` isAlreadyExistsError
        detectFile file (Just oldFileCont)
    context "when we want to override it" $
      it "overrides it" $ \file -> do
        withNewContainer' overrideIfExists file pncDir
        detectFile file (Just newFileCont)
    context "when we want to use it" $
      it "makes it available for editing" $ \file -> do
        withNewContainer' useIfExists file getDirCont `shouldReturn` oldDirCont
        detectFile file (Just oldFileCont)

  context "when container does not exist" . beforeWith missingFile $ do
    context "when we throw" $ do
      context "when we don't want corpse" $
        it "propagates exception, the corpse is removed" $ \file -> do
          withNewContainer' mempty file (pncDir >=> throwExc)
            `shouldThrow` isExc
          detectFile file Nothing
          detectDirCorpse file False
      context "when we want corpse" $
        it "propagates exception, the corpse is there" $ \file -> do
          withNewContainer' preserveCorpse file (pncDir >=> throwExc)
            `shouldThrow` isExc
          detectFile file Nothing
          detectDirCorpse file True
    context "when we finish successfully" $
      it "creates the right file, corpse is always removed" $ \file -> do
        withNewContainer' preserveCorpse file pncDir
        detectFile file (Just newFileCont)
        detectDirCorpse file False

  where
    withNewContainer' pbc p =
      withNewContainer unpack pack (tempDir (parent p) <> pbc) p

withExistingContainerSpec :: SpecWith (Path Abs Dir)
withExistingContainerSpec = do

  context "when container exists" . beforeWith populatedFile $ do
    context "when we throw" $ do
      context "when we don't want corpse" $
        it "propagates exception, the corpse is removed" $ \file -> do
          withExistingContainer' mempty file (pncDir >=> throwExc)
            `shouldThrow` isExc
          detectFile file (Just oldFileCont)
          detectDirCorpse file False
      context "when we want corpse" $
        it "propagates exception, the corpse is there" $ \file -> do
          withExistingContainer' preserveCorpse file (pncDir >=> throwExc)
            `shouldThrow` isExc
          detectFile file (Just oldFileCont)
          detectDirCorpse file True
    context "when we finish successfully" $
      it "updates the container, corpse is always removed" $ \file -> do
        withExistingContainer' preserveCorpse file pncDir
        detectFile file (Just newFileCont)
        detectDirCorpse file False

  context "when container is missing" . beforeWith missingFile $
    it "throws the right exception" $ \file -> do
      withExistingContainer' mempty file pncDir
        `shouldThrow` isDoesNotExistError
      detectFile file Nothing

  where
    withExistingContainer' pbc p =
      withExistingContainer unpack pack (tempDir (parent p) <> pbc) p

----------------------------------------------------------------------------
-- Helpers

withSandbox :: ActionWith (Path Abs Dir) -> IO ()
withSandbox = P.withSystemTempDir "plan-b-sandbox"

populatedFile :: Path Abs Dir -> IO (Path Abs File)
populatedFile dir = path <$ writeFile (toFilePath path) oldFileCont
  where path = dir </> preExistingFile

populatedDir :: Path Abs Dir -> IO (Path Abs Dir)
populatedDir dir = do
  P.createDirIfMissing True path
  forM_ oldDirCont $ \file ->
    writeFile (toFilePath $ path </> file) oldFileCont
  return path
  where path = dir </> preExistingDir

missingFile :: Path Abs Dir -> IO (Path Abs File)
missingFile dir = return (dir </> preExistingFile)

missingDir :: Path Abs Dir -> IO (Path Abs Dir)
missingDir dir = return (dir </> preExistingDir)

-- | Unique type of exception that we throw in tests.

data PlanBException = PlanBException
  deriving (Show, Typeable)

instance Exception PlanBException

throwExc :: a
throwExc = throw PlanBException

isExc :: Selector PlanBException
isExc = const True

detectFile :: Path Abs File -> Maybe String -> Expectation
detectFile file mcont = do
  exists <- P.doesFileExist file
  case mcont of
    Nothing ->
      when exists $
        expectationFailure "target file should not exist, but it does"
    Just cont -> do
      unless exists $
        expectationFailure "target file does not exist, but it should"
      acont <- readFile (toFilePath file)
      unless (cont == acont) $
        expectationFailure "contents of target file are incorrect"

detectDir :: Path Abs Dir -> Maybe [Path Rel File] -> Expectation
detectDir dir mcont = do
  exists <- P.doesDirExist dir
  case mcont of
    Nothing ->
      when exists $
        expectationFailure "target dir should not exist, but it does"
    Just cont -> do
      unless exists $
        expectationFailure "target dir does not exist, but it should"
      -- TODO check that contents of the directory are correct

detectFileCorpse :: Path Abs File -> Bool -> Expectation
detectFileCorpse file must = do
  files <- snd <$> P.listDirRecur (parent file)
  null (files \\ [file]) `shouldBe` not must

detectDirCorpse :: Path Abs t -> Bool -> Expectation
detectDirCorpse dir must = undefined -- TODO write it as well

getFileCont :: Path Abs File -> IO String
getFileCont = readFile . toFilePath

getDirCont :: Path Abs Dir -> IO [Path Rel File]
getDirCont = undefined -- TODO write me, please

pncFile :: Path Abs File -> IO ()
pncFile file = writeFile (toFilePath file) newFileCont

pncDir :: Path Abs Dir -> IO ()
pncDir = undefined -- TODO create/overwrite dir structure

unpack :: Path Abs File -> Path Abs Dir -> IO ()
unpack = undefined -- TODO

pack :: Path Abs Dir -> Path Abs File -> IO ()
pack = undefined -- TODO

----------------------------------------------------------------------------
-- Constants

preExistingFile :: Path Rel File
preExistingFile = $(mkRelFile "file.txt")

preExistingDir :: Path Rel Dir
preExistingDir = $(mkRelDir "dir")

oldFileCont :: String
oldFileCont = "old"

newFileCont :: String
newFileCont = "new"

oldDirCont :: [Path Rel File]
oldDirCont = [$(mkRelFile "old-file-0.txt"), $(mkRelFile "old-file-1.txt")]

newDirCont :: [Path Rel File]
newDirCont = [$(mkRelFile "new-file-0.txt"), $(mkRelFile "new-file-1.txt")]
