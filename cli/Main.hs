import Control.Monad
import System.Environment
import Teletorrent

main :: IO ()
main = do
  fs <- getArgs
  tnames <- mapM guessName fs
  cfg <- loadConfig
  let rs = zipWith TorrentFileStuff fs tnames
  forM_ rs \r -> do
    teletorrentIO cfg r
