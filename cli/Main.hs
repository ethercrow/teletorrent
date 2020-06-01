import System.Environment
import System.Exit
import Teletorrent

main :: IO ()
main = do
  fs <- getArgs
  tnames <- mapM guessName fs
  cfg <- loadConfig
  let rs = zipWith TorrentFileStuff fs tnames
  teletorrentIO cfg rs >>= \case
    Right _ -> pure ()
    Left err -> do
      putStrLn err
      exitFailure

