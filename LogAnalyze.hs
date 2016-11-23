import WebLogParse
import System.IO
import System.Environment
import Data.List (sortBy)
import Data.Map (insertWith, fromList, toList)
import Data.Maybe (fromJust, isJust)

main :: IO ()
main = do
  (logformat:filename:command:xs) <- getArgs
  log <- readFile filename
  let entries = getEntries logformat (lines log)
  either (printAccessLogStats command) (printErrorLogStats command) entries

printAccessLogStats :: String -> [AccessLogEntry] -> IO ()
printAccessLogStats command entries = do
  putStrLn "Access Log Stats:"
  print $ topUsers entries

printErrorLogStats :: String -> [ErrorLogEntry] -> IO ()
printErrorLogStats command entries = do
  putStrLn "Error Log Stats:"
  print (entries !! 0)

getEntries :: String -> [String] -> Either [AccessLogEntry] [ErrorLogEntry]
getEntries format loglines
    | format == "common" = Left (map (run parseAccessLogEntry) loglines) -- Common Log Format
                           
{- Find the top 5 users who issued the most requests in the access log -}        
topUsers :: [AccessLogEntry] -> [(UserID, Int)]
topUsers entries = take 5 (sortBy userCompare (toList userMap))
    where userMap = foldl addUser (fromList []) entries
          userCompare (_,c1) (_,c2) | c1 < c2 = GT | c1 == c2 = EQ | otherwise = LT
          addUser m x = if isJust (user x)
                        then insertWith (+) (fromJust (user x)) 1 m
                        else m
