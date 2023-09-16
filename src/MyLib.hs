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

tryGet url =
  do
    r <- get url
    return $ Just $ r ^. Network.Wreq.responseBody
    `E.catch` \(e :: HTTPClient.HttpException) -> return Nothing

downloadAll =
  go 21
  where
    go :: Int -> IO ()
    go n =
      do
        -- 1 - 20
        -- let url = printf "https://perry-rhodan.net/sites/default/files/downloads/neo%d_1920x1080_0.jpg" n

        -- 21 - 30
        let url = printf "https://perry-rhodan.net/sites/default/files/downloads/wp_neo_%d_1920x1080_0.jpg" n

        r <- tryGet url
        case r of
          Nothing -> putStrLn $ "download failed for n = " ++ show n ++ "\nStopping"
          Just body ->
            do
              let filename = last $ splitOn ['/'] url
              action <- B.writeFile filename body
              putStrLn $ "downloaded " ++ filename
              go (succ n)
