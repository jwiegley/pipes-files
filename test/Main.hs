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
import Test.Hspec

main :: IO ()
main = do
    hspec $ do
        describe "Sanity tests" $ do
            it "Finds expected files in project" findsExpected
    hspec . around withSandbox $ do
        beforeWith populateDirTree $ do
            describe "listDirTree" listDirTreeSpec

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

listDirTreeSpec :: SpecWith FilePath
listDirTreeSpec = it "lists directory tree" $ \dir ->
    listDirTree dir `shouldReturn` map (dir </>) (d ++ f)
        where (d, f) = dirTree
