{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MyLib (downloadAll) where

import Control.Exception as E
import Control.Lens
import qualified Data.ByteString.Lazy as B
import Data.List.Split (splitOn)
import qualified Network.HTTP.Client as HTTPClient
import Network.Wreq
import Text.Printf (printf)

mkUrls :: String -> Int -> [String]
mkUrls url lowerBound =
  [printf url n | n <- [lowerBound ..]]

tryGet url =
  do
    r <- get url
    return $ Just $ r ^. Network.Wreq.responseBody
    `E.catch` \(e :: HTTPClient.HttpException) -> return Nothing

downloadAll =
  -- 1 - 20
  -- let urls = mkUrls "https://perry-rhodan.net/sites/default/files/downloads/neo%d_1920x1080_0.jpg" 1
  -- 21 - 30
  let urls = mkUrls "https://perry-rhodan.net/sites/default/files/downloads/wp_neo_%d_1920x1080_0.jpg" 21
   in go urls
  where
    go [] = return ()
    go (u : rest) =
      do
        r <- tryGet u
        case r of
          Nothing -> putStrLn $ "download failed for " ++ show u ++ "\nStopping"
          Just body ->
            do
              let filename = last $ splitOn ['/'] u
              action <- B.writeFile filename body
              putStrLn $ "downloaded " ++ filename
              go rest
