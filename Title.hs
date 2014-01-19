module Title where

import Control.Monad
import Network.Curl
import Text.Regex
import Text.ParserCombinators.Parsec


-- Find URLs in a string
findURLs :: String -> [String]
findURLs s =
    case result of
      Nothing -> []
      Just (_,matched,remainder,_) -> matched : findURLs remainder
    where
      result = matchRegexAll (mkRegex "https?://[^ ]*[^ $]") s

-- Get title of a webpage
getTitle :: String -> IO (Either ParseError String) -- Instead of applying the parser to the IO String, try eliminating the IO wrapper
getTitle url = (parse p "") `liftM` html
    where
      html = liftM snd (curlGetString url [CurlFollowLocation True])
      p = do
          manyTill anyChar (try (string "<title>") <|> try (string "<TITLE>")) -- waste until title
          manyTill anyChar (try (string "</title>") <|> try (string "</TITLE>")) -- last value = result
