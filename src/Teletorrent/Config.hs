module Teletorrent.Config where

import qualified Data.Text as T
import Dhall (FromDhall, auto, input)
import GHC.Generics (Generic)
import System.Environment.XDG.BaseDir

data Config
  = Config
      { remote_user :: String,
        remote_inbox_torrent_dir :: String,
        remote_finished_content_dir :: String,
        remote_finished_torrent_dir :: String,
        remote_host :: String
      }
  deriving (Show, Generic)

instance FromDhall Config

loadConfig :: IO Config
loadConfig = do
  configDir <- getUserConfigDir "teletorrent"
  input auto (T.pack (configDir <> "/config.dhall"))
