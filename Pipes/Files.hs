{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Pipes.Files
    (
    -- * Introduction
    -- $intro

    -- ** Basic comparison with GNU find
    -- $gnufind

    -- ** Performance
    -- $performance

    -- ** Other notes
    -- $notes

    -- * Finding functions
      sourceFindFiles
    , find
    , findFiles
    , findFilesIO
    , findFilePaths
    , FindOptions(..)
    , defaultFindOptions
    , directoryFiles
    , test
    , ltest
    , stat
    , lstat
    , hasStatus

      -- * File path predicates
    , glob
    , regex
    , ignoreVcs

      -- * GNU find compatibility predicates
    , depth_
    , follow_
    , prune_
    , maxdepth_
    , mindepth_
    , ignoreErrors_
    , noIgnoreErrors_
    , amin_
    , atime_
    , anewer_
    , empty_
    , executable_
    , gid_
    , name_
    , getDepth
    , filename_
    , pathname_
    , getEntryPath
    , getRawEntryPath

    -- * File entry predicates (uses stat information)
    , regular
    , directory
    , hasMode
    , executable
    , lastAccessed_
    , lastModified_

    -- * Predicate combinators
    , module Cond
    , (=~)

    -- * Types and type classes
    , FileEntry(..)
    , IsFilePath(..)

    -- * Helper functions for library writers
    , genericFindFiles
    , genericFindFilePaths
    , genericFind
    , genericTest
    , genericLtest
    ) where

import           Control.Applicative
import           Control.Comonad.Trans.Cofree
import qualified Control.Cond as Cond
import           Control.Cond hiding (test)
import           Control.Exception as E
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logic
import           Control.Monad.Morph
import           Control.Monad.Trans.Control
import           Data.Attoparsec.Text as A
import           Data.Bits
import qualified Data.ByteString as B
import           Data.Char (ord)
import           Data.Monoid
import           Data.String (IsString)
import           Data.Text (Text, pack)
import           Data.Time
import           Data.Time.Clock.POSIX
import           Data.Word (Word8)
import           Foreign.C
import           Hierarchy
import           Pipes
import           Pipes.Files.Directory
import           Pipes.Files.Types
import qualified Pipes.Prelude as P
import           Pipes.Safe
import           Prelude
import           System.Directory hiding (executable, findFiles)
import           System.Posix.ByteString.FilePath
import           System.Posix.FilePath
import           System.PosixCompat.Files
import           System.PosixCompat.Types
import           Text.Regex.Posix ((=~))

{- $intro

**find-conduit** is essentially a souped version of GNU find for Haskell,
using a DSL to provide both ease of use, and extensive flexbility.

In its simplest form, let's compare some uses of find to find-conduit.  Bear
in mind that the result of the find function is a conduit, so you're expected
to either sink it to a list, or operate on the file paths as they are yielded.
-}

{- $gnufind

A typical find command:

@
find src -name '*.hs' -type f -print
@

Would in find-conduit be:

@
find "src" (glob \"*.hs\" \<\> regular) $$ mapM_C (liftIO . print)
@

The 'glob' predicate matches the file basename against the globbing pattern,
while the 'regular' predicate matches plain files.

A more complicated example:

@
find . -size +100M -perm 644 -mtime 1
@

Now in find-conduit:

@
let megs = 1024 * 1024
    days = 86400
now <- liftIO getCurrentTime
find \".\" ( fileSize (> 100*megs)
        \<\> hasMode 0o644
        \<\> lastModified (> addUTCTime now (-(1*days)))
         )
@

Appending predicates like this expressing an "and" relationship.  Use '<|>' to
express "or".  You can also negate any predicate:

@
find \".\" (not_ (hasMode 0o644))
@

By default, predicates, whether matching or not, will allow recursion into
directories.  In order to express that matching predicate should disallow
recursion, use 'prune':

@
find \".\" (prune (depth (> 2)))
@

This is the same as using '-maxdepth 2' in find.

@
find \".\" (prune (filename_ (== \"dist\")))
@

This is the same as:

@
find . \\( -name dist -prune \\) -o -print
@
-}

{- $performance

find-conduit strives to make file-finding a well performing operation.  To
this end, a composed Predicate will only call stat once per entry being
considered; and if you prune a directory, it is not traversed at all.

By default, 'find' calls stat for every file before it applies the predicate,
in order to ensure that only one such call is needed.  Sometimes, however, you
know just from the FilePath that you don't want to consider a certain file, or
you want to prune a directory tree.

To support these types of optimized queries, a variant of find is provided
called 'findWithPreFilter'.  This takes two predicates: one that is applied to
only the FilePath, before stat (or lstat) is called; and one that is applied
to the full file information after the stat.
-}

{- $notes

See 'CondT' for more details on the Monad used to build predicates.
-}

getEntryPath :: (Monad m, IsFilePath f) => CondT (FileEntry f) m f
getEntryPath = queries (fromRawFilePath . entryPath)

getRawEntryPath :: Monad m => CondT (FileEntry f) m RawFilePath
getRawEntryPath = queries entryPath

pathname_ :: (Monad m, IsFilePath f) => (f -> Bool) -> CondT (FileEntry f) m ()
pathname_ f = guard . f =<< getEntryPath

-- jww (2014-04-30): This will not perform well for other f's.
filename_ :: (Monad m, IsFilePath f) => (f -> Bool) -> CondT (FileEntry f) m ()
filename_ f = pathname_ (f . fromRawFilePath . takeFileName . getRawFilePath)

getDepth :: Monad m => CondT (FileEntry f) m Int
getDepth = queries entryDepth

modifyFindOptions :: Monad m
                  => (FindOptions -> FindOptions) -> CondT (FileEntry f) m ()
modifyFindOptions f =
    updates $ \e -> e { entryFindOptions = f (entryFindOptions e) }

------------------------------------------------------------------------
-- Workalike options for emulating GNU find.
------------------------------------------------------------------------

depth_ :: Monad m => CondT (FileEntry f) m ()
depth_ = modifyFindOptions $ \opts -> opts { findContentsFirst = True }

follow_ :: Monad m => CondT (FileEntry f) m ()
follow_ = modifyFindOptions $ \opts -> opts { findFollowSymlinks = True }

prune_ :: Monad m => CondT a m ()
prune_ = prune

ignoreErrors_ :: Monad m => CondT (FileEntry f) m ()
ignoreErrors_ =
    modifyFindOptions $ \opts -> opts { findIgnoreErrors = True }

noIgnoreErrors_ :: Monad m => CondT (FileEntry f) m ()
noIgnoreErrors_ =
    modifyFindOptions $ \opts -> opts { findIgnoreErrors = False }

maxdepth_ :: Monad m => Int -> CondT (FileEntry f) m ()
maxdepth_ l = getDepth >>= guard . (<= l)

mindepth_ :: Monad m => Int -> CondT (FileEntry f) m ()
mindepth_ l = getDepth >>= guard . (>= l)

-- xdev_ = error "NYI"

timeComp :: MonadIO m
         => ((UTCTime -> Bool) -> CondT (FileEntry f) m ()) -> Int
         -> CondT (FileEntry f) m ()
timeComp f n = do
    now <- liftIO getCurrentTime
    f (\t -> diffUTCTime now t > fromIntegral n)

amin_ :: MonadIO m => Int -> CondT (FileEntry f) m ()
amin_ n = timeComp lastAccessed_ (n * 60)

atime_ :: MonadIO m => Int -> CondT (FileEntry f) m ()
atime_ n = timeComp lastAccessed_ (n * 24 * 3600)

anewer_ :: (MonadIO m, IsFilePath f) => f -> CondT (FileEntry f) m ()
anewer_ path = do
    e  <- query
    es <- applyStat Nothing
    ms <- liftIO $ getStat Nothing
        e { entryPath   = getRawFilePath path
          , entryStatus = Nothing
          }
    case ms of
        Nothing     -> prune >> error "This is never reached"
        Just (s, _) -> guard $ diffUTCTime (f s) (f es) > 0
  where
    f = posixSecondsToUTCTime . realToFrac . accessTime

-- cmin_ = error "NYI"
-- cnewer_ = error "NYI"
-- ctime_ = error "NYI"

empty_ :: MonadIO m => CondT (FileEntry f) m ()
empty_ = (regular   >> hasStatus ((== 0) . fileSize))
 `mplus` (directory >> hasStatus ((== 2) . linkCount))

executable_ :: MonadIO m => CondT (FileEntry f) m ()
executable_ = executable

gid_ :: MonadIO m => Int -> CondT (FileEntry f) m ()
gid_ n = hasStatus ((== n) . fromIntegral . fileGroup)

{-
group_ name
ilname_ pat
iname_ pat
inum_ n
ipath_ pat
iregex_ pat
iwholename_ pat
links_ n
lname_ pat
mmin_
mtime_
-}

name_ :: (Monad m, IsFilePath f, Eq f) => f -> CondT (FileEntry f) m ()
name_ = filename_ . (==)

{-
newer_ path
newerXY_ ref
nogroup_
nouser_
path_ pat
perm_ mode :: Perm
readable_
regex_ pat
samefile_ path
size_ n :: Size
type_ c
uid_ n
used_ n
user_ name
wholename_ pat
writable_
xtype_ c
-}

------------------------------------------------------------------------

applyStat :: MonadIO m => Maybe Bool -> CondT (FileEntry f) m FileStatus
applyStat mfollow = do
    ms <- liftIO . getStat mfollow =<< query
    case ms of
        Nothing      -> prune >> error "This is never reached"
        Just (s, e') -> const s `liftM` update e'

lstat :: MonadIO m => CondT (FileEntry f) m FileStatus
lstat = applyStat (Just False)

stat :: MonadIO m => CondT (FileEntry f) m FileStatus
stat = applyStat (Just True)

hasStatus :: MonadIO m => (FileStatus -> Bool) -> CondT (FileEntry f) m ()
hasStatus f = guard . f =<< applyStat Nothing

regular :: MonadIO m => CondT (FileEntry f) m ()
regular = hasStatus isRegularFile

executable :: MonadIO m => CondT (FileEntry f) m ()
executable = hasMode ownerExecuteMode

directory :: MonadIO m => CondT (FileEntry f) m ()
directory = hasStatus isDirectory

hasMode :: MonadIO m => FileMode -> CondT (FileEntry f) m ()
hasMode m = hasStatus (\s -> fileMode s .&. m /= 0)

withStatusTime :: MonadIO m
               => (FileStatus -> EpochTime) -> (UTCTime -> Bool)
               -> CondT (FileEntry f) m ()
withStatusTime g f = hasStatus (f . posixSecondsToUTCTime . realToFrac . g)

lastAccessed_ :: MonadIO m => (UTCTime -> Bool) -> CondT (FileEntry f) m ()
lastAccessed_ = withStatusTime accessTime

lastModified_ :: MonadIO m => (UTCTime -> Bool) -> CondT (FileEntry f) m ()
lastModified_ = withStatusTime modificationTime

regex :: (Monad m, IsFilePath f) => String -> CondT (FileEntry f) m ()
regex pat = filename_ ((=~ pat) . getFilePath)

-- | Return all entries, except for those within version-control metadata
--   directories (and not including the version control directory itself either).
ignoreVcs :: (Monad m, IsString f, Eq f, IsFilePath f)
          => CondT (FileEntry f) m ()
ignoreVcs = when_ (filename_ (`elem` vcsDirs)) prune
  where
    vcsDirs = [ ".git", "CVS", "RCS", "SCCS", ".svn", ".hg", "_darcs" ]

-- | Find every entry whose filename part matching the given filename globbing
--   expression.  For example: @glob "*.hs"@.
glob :: (Monad m, IsString f, IsFilePath f, Monoid f)
     => String -> CondT (FileEntry f) m ()
glob g = case parseOnly globParser (pack g) of
    Left e  -> error $ "Failed to parse glob: " ++ e
    Right x -> regex ("^" <> fromTextPath x <> "$")
  where
    globParser :: Parser Text
    globParser = fmap mconcat $ many $
            char '*' *> return ".*"
        <|> char '?' *> return "."
        <|> string "[]]" *> return "[]]"
        <|> (\x y z -> pack ((x:y) ++ [z]))
                <$> char '['
                <*> manyTill anyChar (A.try (char ']'))
                <*> char ']'
        <|> do
            x <- anyChar
            return . pack $ if x `elem` (".()^$" :: String)
                            then ['\\', x]
                            else [x]

-- | Find file entries in a directory tree, recursively, applying the given
--   recursion predicate to the search.  This conduit yields pairs of type
--   @(FileEntry f, a)@, where is the return value from the predicate at each
--   step.
sourceFindFiles :: (MonadIO m, MonadSafe m, IsFilePath f)
                => FindOptions
                -> f
                -> CondT (FileEntry f) m a
                -> Producer (FileEntry f, a) m ()
sourceFindFiles findOptions startPath =
    walkChildren (newFileEntry (getRawFilePath startPath) 0 findOptions)
{-# INLINE sourceFindFiles #-}

walkChildren :: MonadSafe m
             => FileEntry f
             -> CondT (FileEntry f) m a
             -> Producer (FileEntry f, a) m ()
walkChildren !entry !cond = do
    let !path      = B.snoc (entryPath entry) sep
        !opts      = entryFindOptions entry
        !nextDepth = succ (entryDepth entry)
        !worker    = uncurry $ handleEntry opts path nextDepth cond
    if findPreloadDirectories opts
        then do
            !fps <- liftIO $ getDirectoryContentsAndAttrs path
            forM_ fps $ {-mapInput-} undefined (const Nothing) . worker
        else
            for (sourceDirectory path) worker

handleEntry :: MonadSafe m
            => FindOptions
            -> RawFilePath
            -> Int
            -> CondT (FileEntry f) m a
            -> RawFilePath
            -> CUInt
            -> Producer (FileEntry f, a) m ()
handleEntry opts path nextDepth cond !fp !typ = do
    let childPath = B.append path fp
        child     = newFileEntry childPath nextDepth opts

    ((!mres, !mcond), !child') <- lift $ runCondT child cond

    let opts' = entryFindOptions child'
        this = case mres of
            Nothing -> return ()
            Just res
                | findIgnoreResults opts' -> return ()
                | otherwise -> yield (child', res)
        that = case mcond of
            Nothing -> return ()
            Just !cond'
                | typ == 10 ->
                    when (findFollowSymlinks opts) $ do
                        isDir <- liftIO $ statIsDirectory childPath
                        when isDir $ walkChildren child' cond'
                | typ == 4  -> walkChildren child' cond'
                | otherwise -> return ()

    if findContentsFirst opts'
        then that >> this
        else this >> that
{-# INLINE handleEntry #-}

-- | Find file entries in a directory tree, recursively, applying the given
--   recursion predicate to the search.  This conduit yields pairs of type
--   @(FileEntry f, a)@, where is the return value from the predicate at each
--   step.
findFilesIO :: IsFilePath f
            => FindOptions -> f -> CondT (FileEntry f) IO a -> IO ()
findFilesIO findOptions startPath =
    walkChildrenIO (newFileEntry (getRawFilePath startPath) 0 findOptions)

sep :: Word8
sep = fromIntegral (ord '/')

walkChildrenIO :: FileEntry f -> CondT (FileEntry f) IO a -> IO ()
walkChildrenIO !entry !cond = do
    let !path      = B.snoc (entryPath entry) sep
        !opts      = entryFindOptions entry
        !nextDepth = entryDepth entry + 1
    !fps <- getDirectoryContentsAndAttrs path
    if findDepthFirst opts
        then do
            let f _ Nothing  = return ()
                f _ (Just x) = uncurry walkChildrenIO x
            forM_ fps $ handleEntryIO opts path cond nextDepth (f ())
        else do
            let f acc Nothing  = return acc
                f acc (Just x) = return (x:acc)
            dirs <- (\k -> foldM k [] fps) $ \acc ->
                handleEntryIO opts path cond nextDepth (f acc)
            forM_ dirs $ uncurry walkChildrenIO

handleEntryIO :: FindOptions
              -> RawFilePath
              -> CondT (FileEntry f) IO a
              -> Int
              -> (Maybe (FileEntry f, CondT (FileEntry f) IO a) -> IO b)
              -> (RawFilePath, CUInt)
              -> IO b
handleEntryIO opts path cond nextDepth f (!fp, !typ) = do
    let !childPath = B.append path fp
        !child     = newFileEntry childPath nextDepth opts
    ((_, !mcond), !child') <- runCondT child cond
    case mcond of
        Nothing -> f Nothing
        Just !cond'
            | typ == 10 ->
                if findFollowSymlinks opts
                then do
                    isDir <- liftIO $ statIsDirectory childPath
                    f $ if isDir
                        then Just (child', cond')
                        else Nothing
                else f Nothing
            | typ == 4  -> f (Just (child', cond'))
            | otherwise -> f Nothing
{-# INLINE handleEntryIO #-}

-- | Return all files within a directory tree, hierarchically.
directoryFiles :: (MonadPlus m, MonadIO m) => FilePath -> TreeT m FilePath
directoryFiles path = CofreeT $ do
    Right entries <-
        liftIO $ E.try @E.SomeException $ getDirectoryContents path
    entry <- select (filter (`notElem` [".", ".."]) entries)
    let fullPath = path ++ "/" ++ entry
    Right st <- liftIO $ E.try @E.SomeException $ getFileStatus fullPath
    pure $ fullPath :<
        if isDirectory st
        then Just (directoryFiles fullPath)
        else Nothing

genericFindFiles
    :: (MonadIO m, MonadBaseControl IO m,
        MonadThrow m, MonadCatch m, MonadMask m, IsFilePath f)
    => FindOptions
    -> f
    -> CondT (FileEntry f) m a
    -> m ()
genericFindFiles opts path predicate =
    runSafeT $ runEffect $
        sourceFindFiles opts { findIgnoreResults = True } path
            (hoist lift predicate) >-> P.drain
{-# INLINE genericFindFiles #-}

-- | A simpler version of 'findFiles', which yields only 'FilePath' values,
--   and ignores any values returned by the predicate action.
genericFindFilePaths
    :: (MonadIO m, MonadSafe m, IsFilePath f)
    => FindOptions
    -> f
    -> CondT (FileEntry f) m a
    -> Producer f m ()
genericFindFilePaths opts path predicate =
    sourceFindFiles opts path predicate
        >-> P.map (fromRawFilePath . entryPath . fst)
{-# INLINE genericFindFilePaths #-}

-- | Calls 'findFilePaths' with the default set of finding options.
--   Equivalent to @findFilePaths defaultFindOptions@.
genericFind :: (MonadIO m, MonadSafe m, IsFilePath f)
            => f -> CondT (FileEntry f) m a -> Producer f m ()
genericFind = genericFindFilePaths defaultFindOptions
{-# INLINE genericFind #-}

-- | Test a file path using the same type of predicate that is accepted by
--   'findFiles'.
genericTest :: (MonadIO m, IsFilePath f)
            => CondT (FileEntry f) m () -> f -> m Bool
genericTest matcher path =
    Cond.test
        (newFileEntry (getRawFilePath path) 0 defaultFindOptions
            { findFollowSymlinks = True })
        matcher

-- | Test a file path using the same type of predicate that is accepted by
--   'findFiles', but do not follow symlinks.
genericLtest :: (MonadIO m, IsFilePath f)
             => CondT (FileEntry f) m () -> f -> m Bool
genericLtest matcher path =
    Cond.test
        (newFileEntry (getRawFilePath path) 0 defaultFindOptions)
        matcher
{-# INLINE genericLtest #-}

findFiles :: (MonadIO m, MonadBaseControl IO m,
              MonadThrow m, MonadCatch m, MonadMask m)
          => FindOptions
          -> FilePath
          -> CondT (FileEntry FilePath) m a
          -> m ()
findFiles = genericFindFiles
{-# INLINE findFiles #-}

-- | A simpler version of 'findFiles', which yields only 'FilePath' values,
--   and ignores any values returned by the predicate action.
findFilePaths :: (MonadIO m, MonadSafe m)
              => FindOptions
              -> FilePath
              -> CondT (FileEntry FilePath) m a
              -> Producer FilePath m ()
findFilePaths = genericFindFilePaths
{-# INLINE findFilePaths #-}

-- | Calls 'findFilePaths' with the default set of finding options.
--   Equivalent to @findFilePaths defaultFindOptions@.
find :: (MonadIO m, MonadSafe m)
     => FilePath -> CondT (FileEntry FilePath) m a -> Producer FilePath m ()
find = genericFind
{-# INLINE find #-}

-- | Test a file path using the same type of predicate that is accepted by
--   'findFiles'.
test :: MonadIO m => CondT (FileEntry FilePath) m () -> FilePath -> m Bool
test = genericTest

-- | Test a file path using the same type of predicate that is accepted by
--   'findFiles', but do not follow symlinks.
ltest :: MonadIO m => CondT (FileEntry FilePath) m () -> FilePath -> m Bool
ltest = genericLtest
