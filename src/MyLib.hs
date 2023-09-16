{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MyLib (downloadAll) where

import Control.Exception as E
import Control.Lens
import qualified Data.ByteString.Lazy as B
import qualified Network.HTTP.Client as HTTPClient
import Network.Wreq

tryGet url =
  do
    r <- get url
    return $ Just $ r ^. Network.Wreq.responseBody
    `E.catch` \(e :: HTTPClient.HttpException) -> return Nothing

downloadAll =
  go 1
  where
    go n =
      do
        let filename = "neo" ++ show n ++ "_1920x1080_0.jpg"
        let u = "https://perry-rhodan.net/sites/default/files/downloads/" ++ filename
        r <- tryGet u
        case r of
          Nothing -> putStrLn $ "download failed for n = " ++ show n ++ "\nStopping"
          Just body ->
            do
              action <- B.writeFile filename body
              putStrLn $ "downloaded " ++ filename
              go (succ n)
