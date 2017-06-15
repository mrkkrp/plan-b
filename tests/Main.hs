{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}

module Main (main) where

import Control.Arrow ((***))
import Control.Exception
import Control.Monad
import Data.List ((\\), delete, sort)
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
        detectFile (Just oldFileCont) file
    context "when we want to override it" $
      it "overrides it" $ \file -> do
        withNewFile' overrideIfExists file pncFile
        detectFile (Just newFileCont) file
    context "when we want to use it" $
      it "makes it available for editing" $ \file -> do
        withNewFile' useIfExists file getFileCont `shouldReturn` oldFileCont
        detectFile (Just oldFileCont) file

  context "when such file does not exist" . beforeWith missingFile $ do
    context "when we throw" $ do
      context "when we don't want corpse" $
        it "propagates exception, the corpse is removed" $ \file -> do
          withNewFile' mempty file (pncFile >=> throwExc)
            `shouldThrow` isExc
          detectFile Nothing file
          detectCorpse False file
      context "when we want corpse" $
        it "propagates exception, the corpse is there" $ \file -> do
          withNewFile' preserveCorpse file (pncFile >=> throwExc)
            `shouldThrow` isExc
          detectFile Nothing file
          detectCorpse True file
    context "when we finish successfully" $
      it "creates the right file, corpse is always removed" $ \file -> do
        withNewFile' preserveCorpse file pncFile
        detectFile (Just newFileCont) file
        detectCorpse False file

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
          detectFile (Just oldFileCont) file
          detectCorpse False file
      context "when we want corpse" $
        it "propagates exception, the corpse is there" $ \file -> do
          withExistingFile' preserveCorpse file (pncFile >=> throwExc)
            `shouldThrow` isExc
          detectFile (Just oldFileCont) file
          detectCorpse True file
    context "when we finish successfully" $
      it "updates the right file, corpse is always removed" $ \file -> do
        withExistingFile' preserveCorpse file pncFile
        detectFile (Just newFileCont) file
        detectCorpse False file

  context "when target file is missing" . beforeWith missingFile $
    it "throws the right exception" $ \file -> do
      withExistingFile' mempty file pncFile `shouldThrow` isDoesNotExistError
      detectFile Nothing file

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
        detectDir (Just oldDirCont) dir
    context "when we want to override it" $
      it "overrides it" $ \dir -> do
        withNewDir' overrideIfExists dir pncDir
        detectDir (Just newDirCont) dir
    context "when we want to use it" $
      it "makes it available for editing" $ \dir -> do
        withNewDir' useIfExists dir getDirCont `shouldReturn` oldDirCont
        detectDir (Just oldDirCont) dir

  context "when such directory does not exist" . beforeWith missingDir $ do
    context "when we throw" $ do
      context "when we don't want corpse" $
        it "propagates exception, the corpse is removed" $ \dir -> do
          withNewDir' mempty dir (pncDir >=> throwExc)
            `shouldThrow` isExc
          detectDir Nothing dir
          detectCorpse False dir
      context "when we want corpse" $
        it "propagates exception, the corpse is there" $ \dir -> do
          withNewDir' preserveCorpse dir (pncDir >=> throwExc)
            `shouldThrow` isExc
          detectDir Nothing dir
          detectCorpse True dir
    context "when we finish successfully" $
      it "creates the right directory, corpse is always removed" $ \dir -> do
        withNewDir' preserveCorpse dir pncDir
        detectDir (Just newDirCont) dir
        detectCorpse False dir

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
          detectDir (Just oldDirCont) dir
          detectCorpse False dir
      context "when we want corpse" $
        it "propagates exception, the corpse is there" $ \dir -> do
          withExistingDir' preserveCorpse dir (pncDir >=> throwExc)
            `shouldThrow` isExc
          detectDir (Just oldDirCont) dir
          detectCorpse True dir
    context "when we finish successfully" $
      it "updates the right directory, corpse is always removed" $ \dir -> do
        withExistingDir' preserveCorpse dir pncDir
        detectDir (Just newDirCont) dir
        detectCorpse False dir

  context "when target directory is missing" . beforeWith missingDir $
    it "throws the right exception" $ \dir -> do
      withExistingDir' mempty dir pncDir `shouldThrow` isDoesNotExistError
      detectDir Nothing dir

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
        detectFile (Just oldFileCont) file
    context "when we want to override it" $
      it "overrides it" $ \file -> do
        withNewContainer' overrideIfExists file pncDir
        detectFile (Just newFileCont) file
    context "when we want to use it" $
      it "makes it available for editing" $ \file -> do
        withNewContainer' useIfExists file getDirCont `shouldReturn` oldDirCont
        detectFile (Just oldFileCont) file

  context "when container does not exist" . beforeWith missingFile $ do
    context "when we throw" $ do
      context "when we don't want corpse" $
        it "propagates exception, the corpse is removed" $ \file -> do
          withNewContainer' mempty file (pncDir >=> throwExc)
            `shouldThrow` isExc
          detectFile Nothing file
          detectCorpse False file
      context "when we want corpse" $
        it "propagates exception, the corpse is there" $ \file -> do
          withNewContainer' preserveCorpse file (pncDir >=> throwExc)
            `shouldThrow` isExc
          detectFile Nothing file
          detectCorpse True file
    context "when we finish successfully" $
      it "creates the right file, corpse is always removed" $ \file -> do
        withNewContainer' preserveCorpse file pncDir
        detectFile (Just newFileCont) file
        detectCorpse False file

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
          detectFile (Just oldFileCont) file
          detectCorpse False file
      context "when we want corpse" $
        it "propagates exception, the corpse is there" $ \file -> do
          withExistingContainer' preserveCorpse file (pncDir >=> throwExc)
            `shouldThrow` isExc
          detectFile (Just oldFileCont) file
          detectCorpse True file
    context "when we finish successfully" $
      it "updates the container, corpse is always removed" $ \file -> do
        withExistingContainer' preserveCorpse file pncDir
        detectFile (Just newFileCont) file
        detectCorpse False file

  context "when container is missing" . beforeWith missingFile $
    it "throws the right exception" $ \file -> do
      withExistingContainer' mempty file pncDir
        `shouldThrow` isDoesNotExistError
      detectFile Nothing file

  where
    withExistingContainer' pbc p =
      withExistingContainer unpack pack (tempDir (parent p) <> pbc) p

----------------------------------------------------------------------------
-- Helpers

-- | Create sandbox directory to model some situation in it and run some
-- tests. Note that we're using new unique sandbox directory for each test
-- case to avoid contamination and it's unconditionally deleted after test
-- case finishes.

withSandbox :: ActionWith (Path Abs Dir) -> IO ()
withSandbox = P.withSystemTempDir "plan-b-sandbox"

-- | Create “old” file in the sandbox directory (its location is passed as
-- the first parameter). Return location of the file to be used in tests.

populatedFile :: Path Abs Dir -> IO (Path Abs File)
populatedFile dir = path <$ writeFile (toFilePath path) oldFileCont
  where path = dir </> preExistingFile

-- | Create “old” directory in the sandbox directory. Similar to
-- 'populatedFile', but this thing is a bit more complex because it has
-- several files in it.

populatedDir :: Path Abs Dir -> IO (Path Abs Dir)
populatedDir dir = do
  P.createDirIfMissing True path
  forM_ oldDirCont $ \file ->
    writeFile (toFilePath $ path </> file) ""
  return path
  where path = dir </> preExistingDir

-- | Don't create any file, but generate its name and return it, so it can
-- be used in tests.

missingFile :: Path Abs Dir -> IO (Path Abs File)
missingFile dir = return (dir </> preExistingFile)

-- | Similarly, don't create any directory, but generate its name.

missingDir :: Path Abs Dir -> IO (Path Abs Dir)
missingDir dir = return (dir </> preExistingDir)

-- | Unique type of exception that we throw in tests.

data PlanBException = PlanBException
  deriving (Show, Typeable)

instance Exception PlanBException

-- | Throw exception of type 'PlanBException'.

throwExc :: a
throwExc = throw PlanBException

-- | Selects only 'PlanBException'.

isExc :: Selector PlanBException
isExc = const True

-- | If the first argument is 'Nothing', file should not exist, otherwise
-- its content should match given 'String'. Name of file is taken as the
-- second argument.

detectFile :: Maybe String -> Path Abs File -> Expectation
detectFile mcont file = do
  exists <- P.doesFileExist file
  case mcont of
    Nothing ->
      when exists . expectationFailure $
        "target file should not exist, but it does"
    Just cont -> do
      unless exists . expectationFailure $
        "target file does not exist, but it should"
      acont <- readFile (toFilePath file)
      unless (cont == acont) . expectationFailure $
        "contents of target file are incorrect:\n" ++ show acont

-- | If the first argument is 'Nothing', directory should not exist,
-- otherwise its contents should match given collection of files (but no
-- directories allowed in any case). For the sake of simplicity, we do not
-- check contents of those files. Name of directory is taken as the second
-- argument.

detectDir :: Maybe [Path Rel File] -> Path Abs Dir -> Expectation
detectDir mcont dir = do
  exists <- P.doesDirExist dir
  case mcont of
    Nothing ->
      when exists . expectationFailure $
        "target dir should not exist, but it does"
    Just cont -> do
      unless exists . expectationFailure $
        "target dir does not exist, but it should"
      (dirs, files) <- P.listDirRecur dir
#if MIN_VERSION_path(0,6,0)
      files' <- mapM (P.canonicalizePath >=> stripProperPrefix dir) files
#else
      files' <- mapM (P.canonicalizePath >=> stripDir dir) files
#endif
      unless (null dirs && null (cont \\ files')) . expectationFailure $
        "contents of target directory are incorrect:\n" ++
        unlines (show <$> files')

-- | First argument specifies whether there should be anything in the
-- sandbox directory except for given object. The action checks that
-- expectation. Since we put temporary files (in case of failure these are
-- called corpse and are normally deleted) in the same sandbox as target
-- file, this is a reliable way to check if those temporary files have been
-- deleted.

detectCorpse :: Bool -> Path Abs t -> Expectation
detectCorpse must file = do
  items <- uncurry mappend . (fmap toFilePath *** fmap toFilePath)
    <$> P.listDir (parent file)
  if null (delete (toFilePath file) items)
    then when must . expectationFailure $
           "no corpse detected, but it should exist:\n" ++
           unlines (show <$> items)
    else unless must . expectationFailure $
           "there is corpse, although it should not exist:\n" ++
           unlines (show <$> items)

-- | Get contents of file.

getFileCont :: Path Abs File -> IO String
getFileCont = readFile . toFilePath

-- | Get contents of directory, only files and they are sorted.

getDirCont :: Path Abs Dir -> IO [Path Rel File]
getDirCont dir = sort <$>
#if MIN_VERSION_path(0,6,0)
  (P.listDir dir >>= mapM (stripProperPrefix dir) . snd)
#else
  (P.listDir dir >>= mapM (stripDir dir) . snd)
#endif

-- | “Edit” file—overwrite it with new content.

pncFile :: Path Abs File -> IO ()
pncFile file = writeFile (toFilePath file) newFileCont

-- | “Edit” directory—write/overwrite it with new content. This is done by
-- deletion of files listed in 'oldDirCont' and creation of files listed in
-- 'newDirCont'.

pncDir :: Path Abs Dir -> IO ()
pncDir dir = do
  forM_ ((dir </>) <$> oldDirCont) $ \file -> do
    exists <- P.doesFileExist file
    when exists (P.removeFile file)
  forM_ ((dir </>) <$> newDirCont) $ \file ->
    writeFile (toFilePath file) ""

-- | When given file has 'oldFileCont', put 'oldDirCont' into specified
-- directory. Otherwise put 'newDirCont' there.

unpack :: Path Abs File -> Path Abs Dir -> IO ()
unpack file dir = do
  content <- getFileCont file
  forM_ (if content == oldFileCont then oldDirCont else newDirCont) $ \f ->
    writeFile (toFilePath $ dir </> f) ""

-- | When given directory has 'oldDirCont', write file with 'oldFileCont' to
-- specified path. Otherwise put 'newFileCont' there.

pack :: Path Abs Dir -> Path Abs File -> IO ()
pack dir file = do
  content <- getDirCont dir
  writeFile (toFilePath file) $
    if content == oldDirCont then oldFileCont else newFileCont

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
