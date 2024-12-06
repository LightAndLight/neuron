{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Plugin.Plugins.NeuronIgnore
  ( plugin,
    shouldIgnore,
    mandatoryIgnorePats,
  )
where

import Control.Exception (throw, try)
import Data.Default (Default (def))
import qualified Data.Text as T
import Neuron.Plugin.Type (Plugin (..))
import Reflex.Dom.Core (fforMaybe)
import Relude hiding (trace, traceShow, traceShowId)
import System.FilePattern (FilePattern, (?==))
import System.IO.Error (isDoesNotExistError)

plugin :: Plugin ()
plugin =
  def
    { _plugin_filterSources = applyNeuronIgnore
    }

-- | Ignore files based on the top-level .neuronignore file. If the file does
-- not exist, apply the default patterns.
applyNeuronIgnore :: IO (FilePath -> Bool)
applyNeuronIgnore = do
  mContents <- do
    result <- try $ readFileText "./.neuronignore"
    case result of
      Left err
        | isDoesNotExistError err -> pure Nothing
        | otherwise -> throw err
      Right a -> pure $ Just a

  ignorePats :: [FilePattern] <- fmap (mandatoryIgnorePats <>) $ case mContents of
    Just contents -> do
      let ls = T.lines contents
      pure $
        fforMaybe ls $ \(T.strip -> s) -> do
          guard $ not $ T.null s
          -- Ignore line comments
          guard $ not $ "#" `T.isPrefixOf` s
          pure $ toString s
    _ ->
      pure defaultIgnorePats
  pure $ includeDirEntry ignorePats
  where
    defaultIgnorePats =
      [ -- Ignore dotfiles and dotdirs
        "**/.*/**"
        -- Ignore everything under sub directories
        -- "*/*/**"
      ]
    includeDirEntry pats name =
      Just True
        == ( do
               guard $ not $ shouldIgnore pats name
               pure True
           )

shouldIgnore :: [FilePattern] -> FilePath -> Bool
shouldIgnore (fmap ("./" <>) -> pats) fp =
  any (?== fp) pats

mandatoryIgnorePats :: [FilePattern]
mandatoryIgnorePats =
  [ "**/.neuron/**",
    "**/.git/**"
  ]
