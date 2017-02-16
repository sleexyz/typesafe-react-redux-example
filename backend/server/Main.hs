{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import           Data.Function
import           Data.Aeson.Encode.Pretty
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Cors hiding (Origin)
import qualified Data.ByteString.Lazy.Char8 as BSL8

import qualified App.App as App
import qualified App.Model as App

main :: IO ()
main = do
  putStrLn "Writing ./swagger.json"
  BSL8.writeFile "./swagger.json" $ encodePretty App.swagger
  putStrLn "Running on port 8000"
  let mockDB@App.MkMockDB { App.todos } = App.dbEmpty
  app <- App.app mockDB
  run 8000 $ app
    & logStdoutDev
    & cors (const $ Just corsPolicy)

corsPolicy :: CorsResourcePolicy
corsPolicy = simpleCorsResourcePolicy {corsRequestHeaders = ["Content-Type"]}
