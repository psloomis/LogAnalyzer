import WebLogParse
import System.IO
import System.Environment
import Data.List (sortBy)
import Data.Map (insertWith, fromList, toList)
import Data.Maybe (fromJust, isJust)

main :: IO ()
main = do
  (command:filename:xs) <- getArgs
  log <- readFile filename
  let entries = getEntries (lines log)
  printAccessLogStats command entries

printAccessLogStats :: String -> [AccessLogEntry] -> IO ()
printAccessLogStats command entries = do
  putStrLn "Access Log Stats"
  doCommand command entries

doCommand :: String -> [AccessLogEntry] -> IO ()
doCommand command entries
    | command == "users" = users entries
    | command == "files" = files entries
        
getEntries :: [String] -> [AccessLogEntry]
getEntries loglines = map (run parseAccessLogEntry) loglines -- Common Log Format

users :: [AccessLogEntry] -> IO ()
users entries = do
  putStrLn "Most active users:"
  let u = topUsers entries
  if u == []
  then putStrLn "No user information in logs"
  else mapM_ (\x -> putStrLn ((show (fst x)) ++ " made " ++ (show (snd x)) ++ " requests")) u

{- Find the top 5 users who issued the most requests in the access log -}
topUsers :: [AccessLogEntry] -> [(UserID, Int)]
topUsers entries = take 5 (sortBy userCompare (toList userMap))
    where userMap = foldl addUser (fromList []) entries
          userCompare (_,c1) (_,c2) | c1 < c2 = GT | c1 == c2 = EQ | otherwise = LT
          addUser m x = if isJust (user x)
                        then insertWith (+) (fromJust (user x)) 1 m
                        else m

files :: [AccessLogEntry] -> IO ()
files entries = do
  putStrLn "Most requested file:"
  let f = mostRequestedFile entries
  putStrLn ((show (fst f)) ++ " was requested " ++ (show (snd f)) ++ " times")

mostRequestedFile :: [AccessLogEntry] -> (URL, Int)
mostRequestedFile entries = (sortBy cmp (toList fileMap)) !! 0
    where fileMap = foldl updateMap (fromList []) entries
          cmp (_,c1) (_,c2) | c1 < c2 = GT | c1 == c2 = EQ | otherwise = LT
          updateMap m x = insertWith (+) (getUrlFilename (url (request x))) 1 m
