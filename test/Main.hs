{-# LANGUAGE CPP             #-}

module Main where

import Control.Cond
import Control.Monad
import Data.List
import Pipes
import Pipes.Files
import Pipes.Prelude (toListM)
import Pipes.Tree
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.PosixCompat.Files (createSymbolicLink)
import Test.Hspec

main :: IO ()
main = do
    hspec $ do
        describe "Sanity tests" $ do
            it "Finds expected files in project" findsExpected

    hspec . around withSandbox $ do
        beforeWith populateDirTree $ do
            describe "listDirTree" listDirTreeSpec
            describe "listDirTreeCond" listDirTreeCondSpec
  -- This test may fail on windows as unix-compat does not implement
  -- createSymbolicLink.
#ifndef mingw32_HOST_OS
        beforeWith populateCyclicDirTree $
            describe "listDirTreeCyclic" listDirTreeCyclicSpec
#endif

findsExpected :: Expectation
findsExpected = do
    let ignored = ["./.git", "./dist", "./result"]

    let files = winnow (directoryFiles ".") $ do
            path <- query
            liftIO $ putStrLn $ "Considering " ++ path
            when_ (guard_ (`elem` ignored)) $ do
                liftIO $ putStrLn $ "Pruning " ++ path
                prune
            -- equivalently we can say (but won't reach here now)...
            when (path `elem` ignored) $ do
                liftIO $ putStrLn $ "Pruning " ++ path
                prune

            guard_ (".hs" `isInfixOf`)

            -- We only reach the end if we have a file of interest,
            -- however any directories we didn't prune will still be
            -- descended into
            liftIO $ putStrLn "We found a Haskell file!"

    let expected = [ "./Pipes/Files.hs"
                   , "./Pipes/Files/Directory.hs"
                   , "./Pipes/Files/Types.hs"
                   , "./Setup.hs"
                   , "./test/Main.hs"
                   , "./test/doctest.hs"
                   , "./test/find-hs.hs"
                   ]
    found <- toListM $ enumerate (walk files)
    sort found `shouldBe` expected

withSandbox :: ActionWith FilePath -> IO ()
withSandbox = withSystemTempDirectory "pipes-files-test-sandbox"

-- Create a directory tree in a sandbox and test if we can list it
-- correctly. keep it sorted.

dirTree :: ([FilePath], [FilePath])
dirTree =
  (
      [ "a"
      , "b"
      , "b/c"
      ],
      [
        "b/c/three.txt"
      , "b/two.txt"
      , "one.txt"
      ]
  )

populateDirTree :: FilePath -> IO FilePath
populateDirTree top = do
  let (dirs, files) = dirTree
      pdir          = top </> "pdir"
      withinSandbox = (pdir </>)
  createDirectoryIfMissing True pdir
  forM_ dirs (createDirectoryIfMissing True . withinSandbox)
  forM_ files $ (`writeFile` "") . withinSandbox
  return pdir

listDirTree :: FilePath -> IO [FilePath]
listDirTree dir = toListM (enumerate (walk files)) >>= return . sort
    where files = directoryFiles dir

verifyDirTree :: ([FilePath], [FilePath]) -> FilePath -> Expectation
verifyDirTree expected dir =
    listDirTree dir `shouldReturn` map (dir </>) (d ++ f)
        where (d, f) = expected

listDirTreeSpec :: SpecWith FilePath
listDirTreeSpec = it "lists directory tree" (verifyDirTree dirTree)

listDirTreeCond :: FilePath -> IO [FilePath]
listDirTreeCond dir = toListM (enumerate (walk files)) >>= return . sort
    where
        files = winnow (directoryFiles dir) $ do
            -- TODO we can add multiple conditions in a single test
            --  We can also add separate test cases for more conditionals
            when_ (guardM_ (genericTest directory)) prune

listDirTreeCondSpec :: SpecWith FilePath
listDirTreeCondSpec = it "lists directory tree using conditionals" $ \dir ->
    listDirTreeCond dir `shouldReturn` map (dir </>) (d ++ f)
        where (d, f) =
                    (
                      [
                      --  "a"
                      --, "b"
                      --, "b/c"
                      ],
                      [
                      --  "b/c/three.txt"
                      --, "b/two.txt"
                        "one.txt"
                      ]
                    )

-- | Create a directory structure which has cycles in it due to directory
-- symbolic links.
--
-- 1) Mutual cycles between two directory trees. If we traverse a or c we
-- will get into the same cycle:
    -- a/(b -> c), c/(d -> a)
    -- c/(d -> a), a/(b -> c)
-- 2) Cycle with own ancestor
    -- e/f/(g -> e)

cyclicDirTree :: ([FilePath], [FilePath])
cyclicDirTree =
  (
      [
        "a"
      , "a/b"   -- b points to c
      , "a/b/d" -- because b is same as c
      , "c"
      , "c/d"   -- d points to a
      , "c/d/b" -- because d is same as a
      , "e"
      , "e/f"
      , "e/f/g" -- g points to e
      ],
      []
  )

-- | Created the objects described in 'populatedCyclicDirStructure'.
-- Return path to that directory.

populateCyclicDirTree :: FilePath -> IO FilePath
populateCyclicDirTree top = do
  let pdir          = top </> "pdir"
      withinSandbox = (pdir </>)

  createDirectoryIfMissing True pdir
  createDirectoryIfMissing True $ withinSandbox "a"
  createDirectoryIfMissing True $ withinSandbox "c"
  createDirectoryIfMissing True $ withinSandbox "e/f"

  createSymbolicLink "../c"    $ withinSandbox "a/b"
  createSymbolicLink "../a"    $ withinSandbox "c/d"
  createSymbolicLink "../../e" $ withinSandbox "e/f/g"

  return pdir

listDirTreeCyclicSpec :: SpecWith FilePath
listDirTreeCyclicSpec = it "lists directory trees having traversal cycles"
                           (verifyDirTree cyclicDirTree)
