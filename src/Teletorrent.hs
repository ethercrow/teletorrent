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

import qualified Data.ByteString.Lazy as BS
import Data.Function
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Torrent
import Polysemy
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
  Member (Reader TorrentFileStuff) r =>
  Member RemoteBox r =>
  Member LocalFilesystem r =>
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
