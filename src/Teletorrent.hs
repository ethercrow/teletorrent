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

import Data.Foldable
import qualified Data.ByteString.Lazy as BS
import Data.Function
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Torrent
import Polysemy
import Polysemy.Error
import Polysemy.Reader
import Polysemy.Trace
import Teletorrent.Config
import Teletorrent.Effects.LocalFilesystem
import Teletorrent.Effects.RemoteBox

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

teletorrent ::
  Member (Reader [TorrentFileStuff]) r =>
  Member RemoteBox r =>
  Member LocalFilesystem r =>
  Member (Error String) r =>
  Sem r ()
teletorrent = do
  stuffs <- ask
  for_ stuffs \(TorrentFileStuff f n) -> do
    transferToRemoteInbox f
    waitUntilPathExists (n <> ".torrent")
    transferFrom n "."
  for_ stuffs \(TorrentFileStuff f _) -> do
    removeTorrentFile f

teletorrentIO :: Config -> [TorrentFileStuff] -> IO (Either String ())
teletorrentIO cfg tfs =
  teletorrent
    & removeTorrentFileToIO
    & remoteBoxToIO
    & runReader tfs
    & runReader cfg
    & traceToIO
    & runError
    & runM
