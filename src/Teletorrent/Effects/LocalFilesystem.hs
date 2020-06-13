{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Teletorrent.Effects.LocalFilesystem where

import Control.Monad
import Data.Kind
import Polysemy
import Polysemy.Trace
import System.Exit
import System.Process.Typed

data LocalFilesystem (m :: Type -> Type) (a :: Type) where
  RemoveTorrentFile :: FilePath -> LocalFilesystem m ()
  PrintLocalDiskSpace :: LocalFilesystem m ()

makeSem_ ''LocalFilesystem

removeTorrentFile :: Member LocalFilesystem r => FilePath -> Sem r ()
printLocalDiskSpace :: Member LocalFilesystem r => Sem r ()

removeTorrentFileToIO :: (Member (Embed IO) r, Member Trace r) => Sem (LocalFilesystem : r) a -> Sem r a
removeTorrentFileToIO = interpret \case
  RemoveTorrentFile f -> do
    exitCode <- embed $ runProcess (proc "rm" ["-i", f])
    when (exitCode /= ExitSuccess) $ do
      trace (show exitCode)
  PrintLocalDiskSpace -> do
    exitCode <- embed $ runProcess (proc "df" ["-ha", "."])
    when (exitCode /= ExitSuccess) $ do
      trace (show exitCode)
