{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Teletorrent.Effects.RemoteBox where

import Control.Concurrent
import Control.Monad.Loops
import Data.Kind
import Polysemy
import Polysemy.Reader
import Polysemy.Trace
import System.Exit
import System.Process.Typed

import Teletorrent.Config

data RemoteBox (m :: Type -> Type) (a :: Type) where
  TransferToRemoteInbox :: FilePath -> RemoteBox m ()
  TransferFrom :: FilePath -> FilePath -> RemoteBox m ()
  WaitUntilPathExists :: FilePath -> RemoteBox m ()

makeSem_ ''RemoteBox

transferToRemoteInbox :: Member RemoteBox r => FilePath -> Sem r ()

transferFrom :: Member RemoteBox r => FilePath -> FilePath -> Sem r ()

waitUntilPathExists :: Member RemoteBox r => FilePath -> Sem r ()

remoteBoxToIO :: (Member (Reader Config) r, Member (Embed IO) r, Member Trace r) => Sem (RemoteBox : r) a -> Sem r a
remoteBoxToIO = interpret \case
  TransferToRemoteInbox src -> do
    Config {..} <- ask
    let to = concat [remote_user, "@", remote_host, ":", remote_inbox_torrent_dir, "/"]
    exitCode <- embed $ runProcess (proc "scp" [src, to])
    trace (show exitCode)
  TransferFrom src dst -> do
    Config {..} <- ask
    let from = concat [remote_user, "@", remote_host, ":\"", remote_finished_content_dir, "/", src, "\""]
    exitCode <- embed $ runProcess (proc "rsync" ["-P", "-a", "-e", "ssh", from, dst])
    trace (show exitCode)
  WaitUntilPathExists p -> do
    Config {..} <- ask
    let remote = remote_user <> "@" <> remote_host
        isReady = do
          exitCode <- runProcess (proc "ssh" [remote, "ls '" <> remote_finished_torrent_dir <> "/" <> p <> "'"])
          pure $ case exitCode of
            ExitSuccess -> True
            _ -> False
    embed $ do
      whileM_ (not <$> isReady) $
        threadDelay 10_000_000

