{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Teletorrent.Effects.LocalFilesystem where

import Control.Monad
import Data.Kind
import Polysemy
import Polysemy.Trace
import System.Process.Typed
import System.Exit

data LocalFilesystem (m :: Type -> Type) (a :: Type) where
  RemoveTorrentFile :: FilePath -> LocalFilesystem m ()

makeSem_ ''LocalFilesystem

removeTorrentFile :: Member LocalFilesystem r => FilePath -> Sem r ()

removeTorrentFileToIO :: (Member (Embed IO) r, Member Trace r) => Sem (LocalFilesystem : r) a -> Sem r a
removeTorrentFileToIO = interpret \case
  RemoveTorrentFile f -> do
    (exitCode, out, err) <- embed $ readProcess (proc "rm" ["-i", f])
    when (exitCode /= ExitSuccess) $ do
      trace (show exitCode)
      trace "out:"
      trace $ show out
      trace "err:"
      trace $ show err
