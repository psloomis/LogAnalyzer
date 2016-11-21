import WebLogParse
import System.IO
import System.Environment

main :: IO ()
main = do
  (logformat:filename:command:xs) <- getArgs
  log <- readFile filename
  let entries = getEntries logformat (lines log)
  either printAccessLogStats printErrorLogStats entries

printAccessLogStats :: [AccessLogEntry] -> IO ()
printAccessLogStats entries = do
  putStrLn "Access Log:"
  print (entries !! 0)

printErrorLogStats :: [ErrorLogEntry] -> IO ()
printErrorLogStats entries = do
  putStrLn "Error Log"
  print entries

getEntries :: String -> [String] -> Either [AccessLogEntry] [ErrorLogEntry]
getEntries format loglines
    | format == "common" = Left (map (run parseAccessLogEntry) loglines) -- Common Log Format
