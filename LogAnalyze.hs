import WebLogParse
import System.IO
import System.Environment
import Data.List (sortBy)
import qualified Data.Set as Set (size, union, fromList, toList)
import qualified Data.Map as Map (insertWith, fromList, toList)
import Data.Maybe (fromJust, isJust, mapMaybe)
import Data.Time
import Text.Printf
import Graphics.EasyPlot
import Graphics.Google.Chart

main :: IO ()
main = do
  (command:filename:xs) <- getArgs
  h <- openFile filename ReadMode
  hSetEncoding h utf8_bom
  log <- hGetContents h
  let entries = getEntries (lines log)
  doCommand command entries

commandHelp = "Available commands:\n 'top_files' - show the most requested files\n 'top_users' - show most active users\n 'responses' - http response percentages\n 'plot_req' - plot a graph of http requests over time\n 'plot_users' - plot a line graph of unique monthly users"

doCommand :: String -> [AccessLogEntry] -> IO ()
doCommand command entries
    | command == "top_users" = topUsers entries
    | command == "top_files" = topFiles entries
    | command == "responses" = responseStats entries
    | command == "plot_req" = plotRequests entries
    | command == "plot_users" = plotMonthlyUsers entries
    | otherwise = putStrLn ("invalid command: " ++ command ++ "\n" ++ commandHelp)
        
getEntries :: [String] -> [AccessLogEntry]
getEntries loglines = mapMaybe (run parseAccessLogEntry) loglines -- Common Log Format

plotMonthlyUsers :: [AccessLogEntry] -> IO ()
plotMonthlyUsers entries = do
  putStrLn $ chartURL $
           setTitle "Unique Monthly Users" $
           setSize 500 500 $
           setData (encodeDataSimple [(uniqueUsersPerMonth entries)]) $
           newLineChart
                      
{- Get an association list that maps a month to the number of unique users in that month -}
uniqueUsersPerMonth :: [AccessLogEntry] -> [Int]
uniqueUsersPerMonth entries = map (\x -> Set.size (snd x)) (Map.toList monthlyUsers)
    where userMap m x = let (year, month, _) = toGregorian $ utctDay (time x)
                        in if isJust (user x)
                           then Map.insertWith (Set.union) (year, month) (Set.fromList [(user x)]) m
                           else Map.insertWith (Set.union) (year, month) (Set.fromList []) m
          monthlyUsers = foldl userMap (Map.fromList []) entries

{- Plot a graph of requests over time, where each point represents the number of requests on a given day -}
plotRequests :: [AccessLogEntry] -> IO ()
plotRequests entries = do
  let dayCounts = snd (unzip (requestsPerDay entries))
  let listData = (zip ([1.0 .. (fromIntegral (length dayCounts))] :: [Double]) dayCounts)
  plot (PNG "requests.png") $ Data2D [Title "Days since start (x axis) VS Number of HTTP Requests (y axis)", Style Lines, Color Red] [] (listData :: [(Double,Double)])
  putStrLn "Plotting complete. Output graph in 'requests.png'"
  
{- Get an association list with the number of requests on each day -}
requestsPerDay :: [AccessLogEntry] -> [(Day, Double)]
requestsPerDay entries = Map.toList (foldl (\m x -> Map.insertWith (+) (utctDay (time x)) 1 m) (Map.fromList []) entries)

{- Print the most active users in the web log -}
topUsers :: [AccessLogEntry] -> IO ()
topUsers entries = do
  putStrLn "Most active users:"
  let u = mostActiveUsers entries
  if u == []
  then putStrLn "No user information in logs"
  else mapM_ (\x -> putStrLn ((show (fst x)) ++ " made " ++ (show (snd x)) ++ " requests")) u

{- Find the top 5 users who issued the most requests in the access log -}
mostActiveUsers :: [AccessLogEntry] -> [(UserID, Int)]
mostActiveUsers entries = take 5 (sortBy userCompare (Map.toList userMap))
    where userMap = foldl addUser (Map.fromList []) entries
          userCompare (_,c1) (_,c2) | c1 < c2 = GT | c1 == c2 = EQ | otherwise = LT
          addUser m x = if isJust (user x)
                        then Map.insertWith (+) (fromJust (user x)) 1 m
                        else m
                             
{- Print the top 5 most requested static files -}
topFiles :: [AccessLogEntry] -> IO ()
topFiles entries = do
  putStrLn "Most requested files:"
  let files = mostRequestedFiles entries
  mapM_ (\x -> putStrLn ((show (fst x)) ++ ": " ++ (show (snd x)) ++ " requests")) files

{- Find the most requested files -}
mostRequestedFiles :: [AccessLogEntry] -> [(URL, Int)]
mostRequestedFiles entries = take 5 (sortBy cmp (Map.toList fileMap))
    where fileMap = foldl updateMap (Map.fromList []) entries
          cmp (_,c1) (_,c2) | c1 < c2 = GT | c1 == c2 = EQ | otherwise = LT
          updateMap m x = if (getUrlFilename (url (request x))) == []
                          then m
                          else Map.insertWith (+) (getUrlFilename (url (request x))) 1 m

{- Print the percentage of each HTTP response out of all the types of responses -}
responseStats :: [AccessLogEntry] -> IO ()
responseStats entries = do
  putStrLn "HTTP response statuses by percentage:"
  let counts = responseCounts entries
  let totalResponses = foldl (\total x -> total + (snd x)) 0 counts
  mapM_ (\x -> putStrLn ("status " ++ (show (fst x)) ++ ":\t" ++ (formatPercent (snd x) totalResponses) ++ "%")) counts

formatPercent :: Double -> Double -> String
formatPercent n d = printf "%.2f" (100 * (n / d))

{- Find the number of times each HTTP response is returned in the logs -}
responseCounts :: [AccessLogEntry] -> [(Int, Double)]
responseCounts entries = Map.toList (foldl updateMap (Map.fromList []) entries)
    where updateMap m x = Map.insertWith (+) (status x) 1.0 m
