{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Teletorrent
  ( teletorrent,
    teletorrentIO,
    TorrentFileStuff (..),
    guessName,
    loadConfig,
  )
where

import Control.Concurrent
import Control.Monad.Loops
import qualified Data.ByteString.Lazy as BS
import Data.Function
import Data.Kind
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Torrent
import qualified Dhall
import GHC.Generics (Generic)
import Polysemy
import Polysemy.Reader
import Polysemy.Trace
import System.Environment.XDG.BaseDir
import System.Exit
import System.Process.Typed

data TorrentFileStuff
  = TorrentFileStuff
      { _path :: FilePath,
        _name :: String
      }

guessName :: FilePath -> IO String
guessName torrentFilePath = do
  torrentBS <- BS.readFile torrentFilePath
  let Right (tInfo -> info) = readTorrent torrentBS
  pure (T.unpack (T.decodeUtf8 (BS.toStrict (tName info))))

data Config
  = Config
      { remote_user :: String,
        remote_inbox_torrent_dir :: String,
        remote_finished_content_dir :: String,
        remote_finished_torrent_dir :: String,
        remote_host :: String
      }
  deriving (Show, Generic)

instance Dhall.Interpret Config

loadConfig :: IO Config
loadConfig = do
  configDir <- getUserConfigDir "teletorrent"
  Dhall.input Dhall.auto (T.pack (configDir <> "/config.dhall"))

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

data RemoveTorrentFile (m :: Type -> Type) (a :: Type) where
  RemoveTorrentFile :: FilePath -> RemoveTorrentFile m ()

makeSem_ ''RemoveTorrentFile

removeTorrentFile :: Member RemoveTorrentFile r => FilePath -> Sem r ()

removeTorrentFileToIO :: (Member (Embed IO) r, Member Trace r) => Sem (RemoveTorrentFile : r) a -> Sem r a
removeTorrentFileToIO = interpret \case
  RemoveTorrentFile f -> do
    exitCode <- embed $ runProcess (proc "rm" ["-i", f])
    trace (show exitCode)

teletorrent ::
  Member (Reader TorrentFileStuff) r =>
  Member RemoteBox r =>
  Member RemoveTorrentFile r =>
  Sem r ()
teletorrent = do
  TorrentFileStuff f n <- ask
  transferToRemoteInbox f
  waitUntilPathExists (n <> ".torrent")
  transferFrom n "."
  removeTorrentFile f

teletorrentIO :: Config -> TorrentFileStuff -> IO ()
teletorrentIO cfg tfs =
  teletorrent
    & removeTorrentFileToIO
    & remoteBoxToIO
    & runReader tfs
    & runReader cfg
    & traceToIO
    & runM
