module WebLogParse (parseAccessLogEntry,
                    run,
                    AccessLogEntry,
                    ErrorLogEntry,
                    IP,
                    UserID,
                    HttpStatus,
                    Request,
                    RequestMethod) where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as Token
import Text.Parsec.Language (haskellDef)
import Data.Time
import Data.Time.Format

-- Use the Parsec Language library to create a function for parsing identifiers (user names in the access logs)
lexer = Token.makeTokenParser haskellDef
identifier = Token.identifier lexer
    
data IP = IP String
          deriving (Show)

data HttpStatus = HttpStatus Int
                  deriving (Show)

data UserID = UserID String
              deriving (Show)

{- The 'RequestMethod' data below is based on the 'Method' data in chapter 16 of Real World Haskell, 'Parsing an HTTP Request' section
http://book.realworldhaskell.org/read/using-parsec.html
extended from the original to include all the request methods, rather than just GET and POST -}
data RequestMethod = Get | Post | Put | Delete | Options | Connect | Trace | Head
                     deriving (Eq, Ord, Show)

{- The 'Request' data is based on the 'HttpRequest' data in chapter 16 of Real World Haskell, in the 'Parsing an HTTP Request' section
http://book.realworldhaskell.org/read/using-parsec.html
modified to only store a request method and url -}
data Request = Request {
  method :: RequestMethod
  , url :: String
  } deriving (Eq, Show)

data ErrorLogEntry = ErrorLogEntry String
                     deriving (Show)

data AccessLogEntry = AccessLogEntry {
  ip :: IP
  , user :: Maybe UserID
  , time :: UTCTime
  , request :: Request
  , status :: HttpStatus
  , size :: Int
  } deriving (Show)

parseAccessLogEntry :: Parser AccessLogEntry
parseAccessLogEntry = do { ip <- parseIP
                         ; spaces
                         ; identifier <|> string "-" -- RFC 1413 identity of client
                         ; spaces
                         ; user <- parseUserID
                         ; spaces
                         ; dateString <- parseDateString
                         ; spaces
                         ; request <- parseRequest
                         ; spaces
                         ; status <- many1 digit
                         ; spaces
                         ; size <- many1 digit
                         ; return (AccessLogEntry ip user (parseEntryTime dateString) request (HttpStatus (read status :: Int)) (read size :: Int))
                         }

parseRequest :: Parser Request
parseRequest = do { char '"'
                  ; m <- parseMethod
                  ; spaces
                  ; url <- manyTill anyChar (try (string " HTTP/"))
                  ; digit ; char '.' ; digit
                  ; char '"'
                  ; return (Request m url)
                  }

parseMethod :: Parser RequestMethod
parseMethod = do { try (string "GET")
                 ; return Get
                 }
              <|> do { try (string "POST")
                     ; return Post
                     }
              <|> do { try (string "PUT")
                     ; return Put
                     }
              <|> do { try (string "DELETE")
                     ; return Delete
                     }
              <|> do { try (string "OPTIONS")
                     ; return Options
                     }
              <|> do { try (string "CONNECT")
                     ; return Connect
                     }
              <|> do { try (string "TRACE")
                     ; return Trace
                     }
              <|> do { try (string "HEAD")
                     ; return Head
                     }
              <?> "http request method"

{- This 'parseIP' parser is based on the 'parseIP' parser from this blog post - https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/attoparsec - modified to pass the IP address as a string to the IP data constructor -}
parseIP :: Parser IP
parseIP = do { n1 <- many1 digit
             ; char '.'
             ; n2 <- many1 digit
             ; char '.'
             ; n3 <- many1 digit
             ; char '.'
             ; n4 <- many1 digit
             ; return (IP (n1 ++ "." ++ n2 ++ "." ++ n3 ++ "." ++ n4))
             }

parseDateString :: Parser String
parseDateString = do { char '['
                     ; dateString <- manyTill anyChar (try (char ']'))
                     ; return dateString
                     }

parseEntryTime :: String -> UTCTime
parseEntryTime time = parseTimeOrError True defaultTimeLocale "%d/%b/%Y:%H:%M:%S %z" time :: UTCTime

parseUserID :: Parser (Maybe UserID)
parseUserID = do { user <- identifier
                 ; return $ Just (UserID user)
                 }
              <|> do { try (char '-')
                     ; return Nothing
                     }
              <?> "user id"

{-
From the 'Running a parser' section in the Parsec documentation - https://web.archive.org/web/20140529211116/http://legacy.cs.uu.nl/daan/download/parsec/parsec.html#identStart
modified to return the value that is parsed, rather than IO
-}
run :: Show a => Parser a -> String -> a
run p input
        = case (parse p "" input) of
                        Right x  -> x
