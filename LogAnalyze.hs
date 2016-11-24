import WebLogParse
import System.IO
import System.Environment
import Data.List (sortBy)
import Data.Map (insertWith, fromList, toList)
import Data.Maybe (fromJust, isJust, mapMaybe)
import Text.Printf

main :: IO ()
main = do
  (command:filename:xs) <- getArgs
  h <- openFile filename ReadMode
  hSetEncoding h utf8_bom
  log <- hGetContents h
  let entries = getEntries (lines log)
  doCommand command entries

commandHelp = "Available commands:\n 'top_files' - show the most requested files\n 'top_users' - show most active users\n 'responses' - http response percentages"

doCommand :: String -> [AccessLogEntry] -> IO ()
doCommand command entries
    | command == "top_users" = topUsers entries
    | command == "top_files" = topFiles entries
    | command == "responses" = responseStats entries
    | otherwise = putStrLn ("invalid command: " ++ command ++ "\n" ++ commandHelp)
        
getEntries :: [String] -> [AccessLogEntry]
getEntries loglines = mapMaybe (run parseAccessLogEntry) loglines -- Common Log Format

topUsers :: [AccessLogEntry] -> IO ()
topUsers entries = do
  putStrLn "Most active users:"
  let u = mostActiveUsers entries
  if u == []
  then putStrLn "No user information in logs"
  else mapM_ (\x -> putStrLn ((show (fst x)) ++ " made " ++ (show (snd x)) ++ " requests")) u

{- Find the top 5 users who issued the most requests in the access log -}
mostActiveUsers :: [AccessLogEntry] -> [(UserID, Int)]
mostActiveUsers entries = take 5 (sortBy userCompare (toList userMap))
    where userMap = foldl addUser (fromList []) entries
          userCompare (_,c1) (_,c2) | c1 < c2 = GT | c1 == c2 = EQ | otherwise = LT
          addUser m x = if isJust (user x)
                        then insertWith (+) (fromJust (user x)) 1 m
                        else m

topFiles :: [AccessLogEntry] -> IO ()
topFiles entries = do
  putStrLn "Most requested files:"
  let files = mostRequestedFiles entries
  mapM_ (\x -> putStrLn ((show (fst x)) ++ ": " ++ (show (snd x)) ++ " requests")) files

mostRequestedFiles :: [AccessLogEntry] -> [(URL, Int)]
mostRequestedFiles entries = take 5 (sortBy cmp (toList fileMap))
    where fileMap = foldl updateMap (fromList []) entries
          cmp (_,c1) (_,c2) | c1 < c2 = GT | c1 == c2 = EQ | otherwise = LT
          updateMap m x = if (getUrlFilename (url (request x))) == []
                          then m
                          else insertWith (+) (getUrlFilename (url (request x))) 1 m

responseStats :: [AccessLogEntry] -> IO ()
responseStats entries = do
  putStrLn "HTTP response statuses by percentage:"
  let counts = responseCounts entries
  let totalResponses = foldl (\total x -> total + (snd x)) 0 counts
  mapM_ (\x -> putStrLn ("status " ++ (show (fst x)) ++ ":\t" ++ (formatPercent (snd x) totalResponses) ++ "%")) counts

formatPercent :: Double -> Double -> String
formatPercent n d = printf "%.2f" (100 * (n / d))
                    
responseCounts :: [AccessLogEntry] -> [(Int, Double)]
responseCounts entries = toList (foldl updateMap (fromList []) entries)
    where updateMap m x = insertWith (+) (status x) 1.0 m
