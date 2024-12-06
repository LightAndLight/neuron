{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.CLI.New
  ( newZettelFile,
  )
where

import Colog (WithLog)
import qualified Data.Set as Set
import Data.Some (withSome)
import Data.Text (strip)
import qualified Data.Text as T
import Data.Time.DateMayTime (DateMayTime, formatDateMayTime)
import Neuron.CLI.Logging
import Neuron.CLI.Types (MonadApp, NewCommand (..), getNotesDir)
import Neuron.Reactor.Build (locateZettelFiles)
import Neuron.Zettelkasten.ID (zettelIDSourceFileName)
import qualified Neuron.Zettelkasten.ID.Scheme as IDScheme
import Neuron.Zettelkasten.Resolver (traverseZidsDirTree_)
import Relude
import System.Directory (setCurrentDirectory)
import System.FilePath ((</>))
import qualified System.Posix.Env as Env
import System.Posix.Process (executeFile)

-- | Create a new zettel file and open it in editor if requested
--
-- As well as print the path to the created file.
newZettelFile :: (MonadIO m, MonadApp m, MonadFail m, WithLog env Message m) => NewCommand -> m ()
newZettelFile NewCommand {..} = do
  locateZettelFiles >>= \case
    Left e -> fail $ toString e
    Right (_, t) -> do
      mzid <- withSome idScheme $ \scheme -> do
        val <- liftIO $ IDScheme.genVal scheme
        zettels <-
          flip execStateT mempty $
          traverseZidsDirTree_ (\_fp zid -> modify $ Set.insert zid) t
        pure $ IDScheme.nextAvailableZettelID zettels val scheme
      case mzid of
        Left e -> die $ show e
        Right zid -> do
          notesDir <- getNotesDir
          let zettelFile = zettelIDSourceFileName zid
          liftIO $ do
            fileAction :: FilePath -> FilePath -> IO () <-
              bool (pure showAction) mkEditActionFromEnv edit
            writeFileText (notesDir </> zettelFile) $ defaultZettelContent date
            fileAction notesDir zettelFile
  where
    mkEditActionFromEnv :: IO (FilePath -> FilePath -> IO ())
    mkEditActionFromEnv =
      getEnvNonEmpty "EDITOR" >>= \case
        Nothing ->
          die "\n-e option can only be used with EDITOR environment variable set"
        Just editorCli ->
          pure $ editAction editorCli
    editAction editorCli notesDir zettelFile = do
      -- Show the path first, in case the editor launch fails
      showAction notesDir zettelFile
      setCurrentDirectory notesDir
      executeShellCommand $ editorCli <> " " <> zettelFile
    showAction notesDir zettelFile =
      putStrLn $ notesDir </> zettelFile
    -- Like `executeFile` but takes a shell command.
    executeShellCommand cmd =
      executeFile "bash" True ["-c", cmd] Nothing
    getEnvNonEmpty name =
      Env.getEnv name >>= \case
        Nothing -> pure Nothing
        Just (toString . strip . toText -> v) ->
          if null v then pure Nothing else pure (Just v)

defaultZettelContent :: DateMayTime -> Text
defaultZettelContent (formatDateMayTime -> date) =
  T.intercalate
    "\n"
    [ "---",
      "date: " <> date,
      "---",
      ""
    ]
