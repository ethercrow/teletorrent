{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Teletorrent.Effects.RemoteBox where

import Control.Concurrent
import Control.Monad.Loops
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Kind
import Polysemy
import Polysemy.Error
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

remoteBoxToIO :: (Member (Error String) r, Member (Reader Config) r, Member (Embed IO) r, Member Trace r) => Sem (RemoteBox : r) a -> Sem r a
remoteBoxToIO = interpret \case
  TransferToRemoteInbox src -> do
    trace "Uploading torrent file..."
    Config {..} <- ask
    let to = concat [remote_user, "@", remote_host, ":", remote_inbox_torrent_dir, "/"]
    (exitCode, _out, err) <- embed $ readProcess (proc "scp" [src, to])
    case exitCode of
      ExitSuccess -> trace "Uploading torrent file... Done"
      _ -> do
        trace "Uploading torrent file... Failed"
        trace $ BS.unpack err
        throw $ BS.unpack err
  TransferFrom src dst -> do
    trace "Downloading content..."
    Config {..} <- ask
    let from = concat [remote_user, "@", remote_host, ":\"", remote_finished_content_dir, "/", src, "\""]
    exitCode <- embed $ runProcess (proc "rsync" ["-P", "-a", "-e", "ssh", "--exclude", ".unwanted", from, dst])
    case exitCode of
      ExitSuccess -> trace "Downloading content... Done"
      _ -> do
        trace "Downloading content... Failed"
        throw "Downloading failed"
  WaitUntilPathExists p -> do
    Config {..} <- ask
    trace "Waiting for content to materialize on the remote box..."
    let remote = remote_user <> "@" <> remote_host
        isReady = do
          (exitCode, _, _) <- readProcess (proc "ssh" [remote, "ls '" <> remote_finished_torrent_dir <> "/" <> p <> "'"])
          pure $ case exitCode of
            ExitSuccess -> True
            _ -> False
    embed $ do
      whileM_ (not <$> isReady) $
        threadDelay 10_000_000
