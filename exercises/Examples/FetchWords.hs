import Network.HTTP
import Control.Monad

main = do
  rsp <- simpleHTTP (getRequest "http://httpbin.org/base64/aGFza2VsbCBmb3IgZXZlcgo=")
  body <- getResponseBody rsp
  forM_ (words body) $ \w -> do
     putStr "word: "
     putStrLn w
