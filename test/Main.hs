module Main where

import Control.Cond
import Control.Monad
import Data.List
import Pipes
import Pipes.Files
import Pipes.Prelude (toListM)
import Pipes.Tree
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "Sanity tests" $ do
        it "Finds expected files in project" findsExpected

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
