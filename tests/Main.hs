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
import Path
import System.IO.Error
import System.IO.Temp
import System.PlanB
import Test.Hspec
import qualified Path.IO as Dir

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (<$))
#endif

main :: IO ()
main = hspec . around withSandbox $ do
  describe "withNewFile"           withNewFileSpec
  describe "withExistingFile"      withExistingFileSpec
  -- describe "withNewDir"            withNewDirSpec
  -- describe "withExistingDir"       withExistingDirSpec
  -- describe "withNewContainer"      withNewContainerSpec
  -- describe "withExistingConcainer" withExistingContainerSpec

withNewFileSpec :: SpecWith (Path Abs Dir)
withNewFileSpec = do

  context "when such file already exists" . beforeWith populatedFile $ do
    context "when we want to error" $
      it "throws the right exception" $ \file ->
        withNewFile' mempty file makeit `shouldThrow` isAlreadyExistsError
    context "when we want to override it" $
      it "overrides it" $ \file -> do
        withNewFile' overrideIfExists file makeit
        readFile (toFilePath file) `shouldReturn` newFileCont
    context "when we want to use it" $
      it "makes it available for editing" $ \file ->
        withNewFile' useIfExists file readit `shouldReturn` oldFileCont

  context "when such file does not exists" . beforeWith missingFile $ do
    context "when we throw" $ do
      context "when we want corpse" $
        it "propagates exception, the corpse is there" $ \file -> do
          withNewFile' preserveCorpse file (makeit >=> throwUnderflow)
            `shouldThrow` isUnderflow
          detectFileCorpse file True
      context "when we don't want corpse" $
        it "propagates exception, the corpse is removed" $ \file -> do
          withNewFile' mempty file (makeit >=> throwUnderflow)
            `shouldThrow` isUnderflow
          detectFileCorpse file False
    context "when we finish successfully" $
      it "creates right file, corpse is always removed" $ \file -> do
        withNewFile' preserveCorpse file makeit
        readFile (toFilePath file) `shouldReturn` newFileCont
        detectFileCorpse file False

  where withNewFile' pbc p = withNewFile (tempDir (parent p) <> pbc) p
        makeit path = writeFile (toFilePath path) newFileCont
        readit  = readFile . toFilePath

withExistingFileSpec :: SpecWith (Path Abs Dir)
withExistingFileSpec = do

  context "when target file exists" . beforeWith populatedFile $ do
    context "when we throw" $ do
      context "when we want corpse" $
        it "propagates exception, the corpse is there" $ \file -> do
          withExistingFile' preserveCorpse file (makeit >=> throwUnderflow)
            `shouldThrow` isUnderflow
          detectFileCorpse file True
      context "when we don't want corpse" $
        it "propagates exception, the corpse is removed" $ \file -> do
          withExistingFile' mempty file (makeit >=> throwUnderflow)
            `shouldThrow` isUnderflow
          detectFileCorpse file False
    context "when we finish successfully" $
      it "updates right file, corpse is always removed" $ \file -> do
        withExistingFile' preserveCorpse file makeit
        readFile (toFilePath file) `shouldReturn` newFileCont
        detectFileCorpse file False

  context "when target file is missing" . beforeWith missingFile $
    it "throws the right exception" $ \file ->
      withExistingFile' mempty file makeit `shouldThrow` isDoesNotExistError

  where withExistingFile' pbc p = withExistingFile (tempDir (parent p) <> pbc) p
        makeit path = writeFile (toFilePath path) newFileCont

-- withNewDirSpec :: SpecWith (Path Abs Dir)
-- withNewDirSpec = undefined

-- withExistingDirSpec :: SpecWith (Path Abs Dir)
-- withExistingDirSpec = undefined

-- withNewContainerSpec :: SpecWith (Path Abs Dir)
-- withNewContainerSpec = undefined

-- withExistingContainerSpec :: SpecWith (Path Abs Dir)
-- withExistingContainerSpec = undefined

----------------------------------------------------------------------------
-- Helpers

withSandbox :: ActionWith (Path Abs Dir) -> IO ()
withSandbox action = withSystemTempDirectory "plan-b-sandbox"
  (parseAbsDir >=> action)

populatedFile :: Path Abs Dir -> IO (Path Abs File)
populatedFile dir = path <$ writeFile (toFilePath path) oldFileCont
  where path = dir </> preExistingFile

missingFile :: Path Abs Dir -> IO (Path Abs File)
missingFile dir = return (dir </> preExistingFile)

throwUnderflow :: a
throwUnderflow = throw Underflow

isUnderflow :: Selector ArithException
isUnderflow Underflow = True
isUnderflow _         = False

detectFileCorpse :: Path Abs File -> Bool -> IO ()
detectFileCorpse file must = do
  files <- snd <$> Dir.listDirRecur (parent file)
  null (files \\ [file]) `shouldBe` not must

----------------------------------------------------------------------------
-- Constants

preExistingFile :: Path Rel File
preExistingFile = $(mkRelFile "file.txt")

-- preExistingDir :: Path Rel Dir
-- preExistingDir = $(mkRelDir "dir")

oldFileCont :: String
oldFileCont = "old"

newFileCont :: String
newFileCont = "new"
