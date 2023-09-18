{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Loader (downloadAll) where

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

downloadUrls [] = return ()
downloadUrls (u : rest) =
  do
    r <- tryGet u
    case r of
      Nothing -> putStrLn $ "download failed for " ++ show u ++ "\nStopping"
      Just body ->
        do
          let filename = last $ splitOn ['/'] u
          action <- B.writeFile filename body
          putStrLn $ "downloaded " ++ filename
          downloadUrls rest

downloadAll =
  let urlMakers =
        [ mkUrls "https://perry-rhodan.net/sites/default/files/downloads/neo%d_1920x1080_0.jpg" 1, -- 1 - 20
          mkUrls "https://perry-rhodan.net/sites/default/files/downloads/wp_neo_%d_1920x1080_0.jpg" 21, -- 21 - 30
          mkUrls "https://perry-rhodan.net/sites/default/files/downloads/wp_neo_%d_1920x1080.jpg" 31, -- 31 - 44
          mkUrls "https://perry-rhodan.net/sites/default/files/downloads/wp_neo_%d_1920x768.jpg" 45, -- 45
          mkUrls "https://perry-rhodan.net/sites/default/files/downloads/wp_neo_%d_1920x1080.jpg" 46, -- 46 - 164
          mkUrls "https://perry-rhodan.net/sites/default/files/downloads/wp_neo_%d_1920x1080_0.jpg" 165, -- 165
          mkUrls "https://perry-rhodan.net/sites/default/files/downloads/wp_neo_%d_1920x1080.jpg" 166, -- 166 - 192
          mkUrls "https://perry-rhodan.net/sites/default/files/downloads/pr_neo_%d_wallpaper_1920x1080.jpg" 193, -- 193 - 194
          mkUrls "https://perry-rhodan.net/sites/default/files/downloads/wp_neo_%d_1920x1080.jpg" 195, -- 195 - 201
          mkUrls "https://perry-rhodan.net/sites/default/files/downloads/wp_neo_%d_1920x1080_0.jpg" 202, -- 202
          mkUrls "https://perry-rhodan.net/sites/default/files/downloads/wp_neo_%d_1920x1080.jpg" 203, -- 203 - 220
          mkUrls "https://perry-rhodan.net/sites/default/files/downloads/wp_neo_%d.jpg" 221, -- 221 - 227
          mkUrls "https://perry-rhodan.net/sites/default/files/downloads/wp_neo_%djpg.jpg" 228, -- 228
          mkUrls "https://perry-rhodan.net/sites/default/files/downloads/pr_wallpaper_neo%d.jpg" 229, -- 229
          mkUrls "https://perry-rhodan.net/sites/default/files/downloads/wp_neo_%d_0.jpg" 230, -- 230
          mkUrls "https://perry-rhodan.net/sites/default/files/downloads/wp_neo_%d.jpg" 231, -- 231
          mkUrls "https://perry-rhodan.net/sites/default/files/downloads/wp%d.jpg" 232, -- 232
          mkUrls "https://perry-rhodan.net/sites/default/files/downloads/wp%d_kopie.jpg" 233, -- 233
          mkUrls "https://perry-rhodan.net/sites/default/files/downloads/wp%d_koeln_kopie.jpg" 234, -- 234
          mkUrls "https://perry-rhodan.net/sites/default/files/downloads/wp%d_original_kopie.jpg" 234, -- 234
          mkUrls "https://perry-rhodan.net/sites/default/files/downloads/wp_%d_kopie.jpg" 235, -- 235
          mkUrls "https://perry-rhodan.net/sites/default/files/downloads/wp_neo%d.jpg" 236, -- 236 - 237
          mkUrls "https://perry-rhodan.net/sites/default/files/downloads/wp_pr_neo%d.jpg" 238, -- 238 - 239
          mkUrls "https://perry-rhodan.net/sites/default/files/downloads/wp_neo%d.jpg" 240, -- 240
          mkUrls "https://perry-rhodan.net/sites/default/files/downloads/wp_pr_neo%d.jpg" 241, -- 241 - 244
          mkUrls "https://perry-rhodan.net/sites/default/files/downloads/wp_pr_neo%d_0.jpg" 245, -- 245
          mkUrls "https://perry-rhodan.net/sites/default/files/downloads/wp_pr_neo%d.jpg" 246, -- 246 - 247
          mkUrls "https://perry-rhodan.net/sites/default/files/downloads/wp_neo%d.jpg" 248, -- 248 - 253
          mkUrls "https://perry-rhodan.net/sites/default/files/downloads/wp_neo%d_variant.jpg" 250, -- 250
          mkUrls "https://perry-rhodan.net/sites/default/files/downloads/wp_neo%d.jpg" 251, -- 251 - 253
          mkUrls "https://perry-rhodan.net/sites/default/files/downloads/wp_pr_neo%d.jpg" 254, -- 254 - 263
          mkUrls "https://perry-rhodan.net/sites/default/files/downloads/wp_pr_neo%d_0.jpg" 264, -- 264
          mkUrls "https://perry-rhodan.net/sites/default/files/downloads/wp_pr_neo%d.jpg" 265, -- 265
          mkUrls "https://perry-rhodan.net/sites/default/files/downloads/wp_pr_neo%d_0.jpg" 266, -- 266
          mkUrls "https://perry-rhodan.net/sites/default/files/downloads/wp_pr_neo%d.jpg" 267, -- 267 - 280
          mkUrls "https://perry-rhodan.net/sites/default/files/downloads/wp_pr_neo%d_0.jpg" 281, -- 281 - 283
          mkUrls "https://perry-rhodan.net/sites/default/files/downloads/wp_pr_neo%d.jpg" 284 -- 284 - 313
        ]
   in mapM_ downloadUrls urlMakers
