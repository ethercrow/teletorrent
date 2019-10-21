module Teletorrent.Config where

import qualified Data.Text as T
import Data.Torrent
import qualified Dhall
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

instance Dhall.Interpret Config

loadConfig :: IO Config
loadConfig = do
  configDir <- getUserConfigDir "teletorrent"
  Dhall.input Dhall.auto (T.pack (configDir <> "/config.dhall"))

