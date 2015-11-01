module Lib (main) where

import BasePrelude
import Control.Monad.IO.Class
import Control.Monad.Random (getRandomR)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Vector (Vector, fromList, (!))
import Web.Spock

main :: IO ()
main = do
  port <- fromMaybe 3030
      <$> fmap read
      <$> lookupEnv "PORT"
  list <- fromList
      <$> T.splitOn "%\r\n"
      <$> T.readFile "data/limericks.txt"
  runServer port list

runServer :: Int -> Vector T.Text -> IO ()
runServer port list = runSpock port $ spockT id $ do
  get root $ do
    setHeader "Access-Control-Allow-Origin" "*"
    text =<< liftIO randLimeric
  where
    randLimeric = (list !) <$> getRandomR (0, length list - 1)
