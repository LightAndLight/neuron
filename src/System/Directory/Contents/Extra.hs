{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module System.Directory.Contents.Extra where

import Colog (WithLog, log)
import qualified Data.Map.Strict as Map
import Neuron.CLI.Logging
import Relude
import System.Directory
  ( canonicalizePath
  , copyFile
  , createDirectoryIfMissing
  , doesDirectoryExist
  , doesPathExist
  , getSymbolicLinkTarget
  , listDirectory
  , pathIsSymbolicLink
  )
import qualified System.Directory.Contents as DC
import System.FilePath ((</>), takeDirectory, takeFileName)
import System.Posix (fileExist, getFileStatus, modificationTime)

-- | Copy the given directory tree from @src@ as base directory, to @dest@
rsyncDir :: (MonadIO m, WithLog env Message m) => FilePath -> FilePath -> DC.DirTree FilePath -> m Int
rsyncDir src dest = \case
  DC.DirTree_File fp _ -> do
    let (a, b) = (src </>) &&& (dest </>) $ fp
    aT <- liftIO $ modificationTime <$> getFileStatus a
    -- TODO: if a file gets deleted, we must remove it.
    mBT <- liftIO $ do
      fileExist b >>= \case
        True -> do
          bT <- modificationTime <$> getFileStatus b
          pure $ Just bT
        False ->
          pure Nothing
    if maybe True (aT >) mBT
      then do
        log I $ toText $ "+ " <> fp
        liftIO $ copyFile a b
        pure 1
      else pure 0
  DC.DirTree_Symlink {} ->
    pure 0
  DC.DirTree_Dir dp children -> do
    liftIO $ createDirectoryIfMissing False (dest </> dp)
    fmap sum $
      forM (Map.elems children) $ \childTree -> do
        rsyncDir src dest childTree

{- The following code originated in https://github.com/srid/directory-contents/blob/0d3f1d5c86063232a3ccf081d9be143eb2ff1466/src/System/Directory/Contents.hs#L124,
which was distributed under the BSD3 license:

Copyright (c) 2020, Obsidian Systems LLC

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Obsidian Systems LLC nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

-- | Recursively list the contents of a 'FilePath', representing the results as
-- a hierarchical 'DirTree'. Only entries whose (non-canonical) paths satisfy the predicate
-- will be included.
--
-- This function should produce results similar to
-- the linux command @tree -l@.
--
-- For example, given this directory and symlink structure
-- (as shown by @tree -l@):
--
-- > test
-- > ├── A
-- > │   ├── a
-- > │   ├── A -> ../A  [recursive, not followed]
-- > │   └── B -> ../B
-- > │       ├── A -> ../A  [recursive, not followed]
-- > │       └── b
-- > ├── B
-- > │   ├── A -> ../A  [recursive, not followed]
-- > │   └── b
-- > └── C -> ../C
-- >     └── c
--
-- this function will produce the following (as rendered by 'drawDirTree'):
--
-- > test
-- > |
-- > +- A
-- > |  |
-- > |  +- A -> ../A
-- > |  |
-- > |  +- B -> ../B
-- > |  |
-- > |  `- a
-- > |
-- > +- B
-- > |  |
-- > |  +- A -> ../A
-- > |  |
-- > |  `- b
-- > |
-- > `- C -> ../C
-- >    |
-- >    `- c
--
buildDirTreeFiltered :: (FilePath -> Bool) -> FilePath -> IO (Maybe (DC.DirTree FilePath))
buildDirTreeFiltered predicate root = build Map.empty root
  where
    build seen path = do
      if predicate path
        then do
          canon <- canonicalizePath path
          isPath <- doesPathExist path
          isDir <- doesDirectoryExist path
          let pathExists = isPath || isDir
          isSym <- if pathExists then fmap Just (pathIsSymbolicLink path) else pure Nothing
          subpaths <- if isDir then listDirectory path else pure []
          subcanons <- mapM canonicalizePath <=<
            filterM (fmap not . pathIsSymbolicLink) $ (path </>) <$> subpaths
          let seen' = Map.union seen $ Map.fromList $ zip subcanons subpaths
              buildSubpaths = catMaybes <$> mapM
                (build (Map.insert canon path seen') . (path </>)) subpaths
          if | not isPath -> pure Nothing
             | isSym == Just True -> case Map.lookup canon seen' of
                 Nothing -> do
                   s <- getSymbolicLinkTarget path
                   Just . DC.DirTree_Symlink path . DC.Symlink_External s . DC.fileNameMap <$> buildSubpaths
                 Just _ -> do
                   target <- getSymbolicLinkTarget path
                   canonRoot <- canonicalizePath root
                   let startingPoint = takeFileName root
                   canonSym <- canonicalizePath $ takeDirectory path </> target
                   pure $ Just $ DC.DirTree_Symlink path $ DC.Symlink_Internal target $
                    startingPoint </> DC.mkRelative canonRoot canonSym
             | isDir -> Just . DC.DirTree_Dir path . DC.fileNameMap <$> buildSubpaths
             | otherwise -> pure $ Just $ DC.DirTree_File path path
        else
          pure Nothing
