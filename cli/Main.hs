import System.Environment
import Teletorrent

main :: IO ()
main = do
  [f] <- getArgs
  tname <- guessName f
  cfg <- loadConfig
  let r = TorrentFileStuff f tname
  teletorrentIO cfg r
