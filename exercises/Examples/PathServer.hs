module Examples.PathServer where

import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Types.Status (Status, status200, status404)
import Network.Wai (Application, Response, responseLBS, pathInfo)
import Network.Wai.Handler.Warp (run)

-- helper for constructing Responses
makeResponse :: Status -> T.Text -> Response
makeResponse status text =
  responseLBS status [] (B.fromStrict (encodeUtf8 text))

serveSource :: IO Response
serveSource = do
  source <- readFile "PathServer.hs"
  return $ makeResponse status200 (T.pack source)

serveSecret :: IO Response
serveSecret =
  return $ makeResponse status200 (T.pack "the secret is swordfish")

serveNotFound :: [T.Text] -> IO Response
serveNotFound path =
  let showPath = T.intercalate (T.pack "/") path
      contents = T.append (T.pack "Not found: ") showPath
  in return $ makeResponse status404 contents

-- we can't pattern match on Text, so we use guards and (==)
servePath :: [T.Text] -> IO Response
servePath path
  | path == [T.pack "source"] = serveSource
  | path == [T.pack "secret", T.pack "file"] = serveSecret
  | otherwise = serveNotFound path

-- type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
application :: Application
application request respond = do
  let path = pathInfo request
  response <- servePath path   -- run IO operation to produce response
  respond response

port :: Int
port = 3421

main :: IO ()
main = run port application
