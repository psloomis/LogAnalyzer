import WebLogParse
import System.IO

main :: IO ()
main = do
  log <- readFile "NASA_access_log_Jul95"
  let entries = map (run parseAccessLogEntry) (lines log)
  print (entries !! 0)
