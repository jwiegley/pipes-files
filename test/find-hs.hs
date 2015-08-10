{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Writer
import           Data.ByteString
import qualified Data.ByteString.Char8 as B
import           Data.IORef
import qualified Data.List as L
import qualified Data.Text as T
import           Data.Text.Encoding
import           Pipes
import           Pipes.Files
import qualified Pipes.Prelude as P
import           Pipes.Safe
import qualified Prelude
import           Prelude hiding (putStrLn)
import           System.Environment
import           System.Posix.ByteString.FilePath
import           System.Posix.Process
import           System.Process
import           Test.Hspec
import           Test.Hspec.Expectations

main :: IO ()
main = do
    args <- getArgs
    case args of
        []                              -> compareVersions
        ["hierarchy", dir]           -> hierarchy dir Nothing
        ["hierarchy-preload", dir]   -> hierarchyPreload dir Nothing
        ["hierarchy-io", dir]        -> hierarchyIO dir Nothing
        -- ["hierarchy-gather", dir] -> hierarchyGather dir Nothing
        ["hierarchy-source", dir]    -> hierarchySource dir Nothing
        ["find", dir]                   -> gnuFind dir Nothing
        _                               -> error "Invalid arguments"

gatherFiles :: (Maybe (IORef [RawFilePath]) -> IO ()) -> IO [FilePath]
gatherFiles f = do
    files <- newIORef []
    f (Just files)
    L.sort . Prelude.map (T.unpack . decodeUtf8) <$> readIORef files

compareVersions = hspec $ describe "Comparison tests" $ do
    it "Running findFiles from hierarchy" $ do
        expected <- gatherFiles $ hierarchy "."
        found <- gatherFiles $ hierarchy "."
        found `shouldBe` expected
    -- it "Running findFiles from hierarchy with preload" $ do
    --     expected <- gatherFiles $ hierarchy "."
    --     found <- gatherFiles $ hierarchyPreload "."
    --     found `shouldBe` expected
    it "Running findFilesIO from hierarchy" $ do
        expected <- gatherFiles $ hierarchy "."
        found <- gatherFiles $ hierarchyIO "."
        found `shouldBe` expected
    -- it "Running parFindFiles from hierarchy" $ do
    --     expected <- gatherFiles $ hierarchy "."
    --     found <- gatherFiles $ hierarchyGather "."
    --     found `shouldBe` expected
    it "Running findFilesSource from hierarchy" $ do
        expected <- gatherFiles $ hierarchy "."
        found <- gatherFiles $ hierarchySource "."
        found `shouldBe` expected
    it "Running GNU find" $ do
        expected <- gatherFiles $ hierarchy "."
        found <- gatherFiles $ gnuFind "."
        found `shouldBe` expected

hierarchy dir files = do
    findFiles defaultFindOptions dir $ do
        path <- getRawEntryPath
        guard (".hs" `isSuffixOf` path)
        liftIO $ maybe (B.putStrLn path) (flip modifyIORef' (path :)) files

hierarchyPreload dir files = do
    findFiles defaultFindOptions
            { findPreloadDirectories = True
            }
        dir $ do
            path <- getRawEntryPath
            guard (".hs" `isSuffixOf` path)
            liftIO $ maybe (B.putStrLn path) (flip modifyIORef' (path :)) files

hierarchyIO dir files = do
    findFilesIO defaultFindOptions (encodeUtf8 (T.pack dir)) $ do
        path <- getRawEntryPath
        guard (".hs" `isSuffixOf` path)
        liftIO $ maybe (B.putStrLn path) (flip modifyIORef' (path :)) files

-- hierarchyGather dir files = do
--     gatherFrom 128 (\queue ->
--             findFilesIO defaultFindOptions
--                 (encodeUtf8 (T.pack dir)) $ do
--                     path <- asks entryPath
--                     guard (".hs" `isSuffixOf` path)
--                     norecurse
--                     liftIO $ atomically $ writeTBQueue queue path)
--         $$ mapM_C (liftIO . putStrLn)

hierarchySource dir files = do
    runSafeT $ runEffect $
        for (sourceFindFiles defaultFindOptions
             (encodeUtf8 (T.pack dir)) (return ())
             >-> P.filter ((".hs" `isSuffixOf`) . entryPath . fst)) $ \p -> do
            let path = entryPath (fst p)
            liftIO $ maybe (B.putStrLn path) (flip modifyIORef' (path :)) files

gnuFind dir files = do
    output <- readProcess "find" [dir, "-name", "*.hs", "-print"] ""
    case files of
        Nothing  -> Prelude.putStrLn output
        Just ref -> writeIORef ref (L.map (encodeUtf8 . T.pack) (lines output))
