{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module App.APISpec where

import           Data.Aeson
import           Control.Lens ((&))
import           GHC.Stack
import           Test.Hspec
import           Network.HTTP.Types
import           Network.Wai.Handler.Warp
import           Servant.Swagger
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types as HTTP

import           App.App
import           App.API
import           App.Model
import           App.Types

data TestReq =
    GET_ { path :: String}
  | forall a. (ToJSON a) => POST_ { path :: String, requestBody :: a}

getPath :: TestReq -> String
getPath = path

spec :: Spec
spec = do
  describe "Datatype encodings:" $ do
    context "toJSON works for"$ do
      validateEveryToJSON appApi

  -- fixme: get rid of fictional tests
  describe "API:" $ do
    describe "POST /todos (create a todo)" $ do
      it "FICTIONAL: returns todoId" $ do
        let Check{..} = checkWithMockDB dbEmpty
        POST_ "/todos" dummyTodo ==> Right (encode ("0" :: String))

      it "returns todo" $ do
        pending

    describe "GET /todos/{id}" $ do
      context "when found" $ do
        let Check{..} = checkWithMockDB $ dbEmpty & fst . dbAddTodo dummyTodo
        it "FICTIONAL: returns todo" $ do
          GET_ "/todos/0" ==> Right (encode dummyTodo)

        it "returns todo" $ do
          pending

    describe "POST /todos (update a todo)" $ do
      context "when found" $ do
        it "returns updated todo" $ do
          pending

    describe "DELETE /todos" $ do
      context "when found" $ do
        it "returns todo" $ do
          pending

-- * Test Fixtures

dummyTodo :: Todo
dummyTodo = MkTodo {
  value = "do stuff"
}

-- * Utilities

data Check = Check {(==>) :: (?loc :: CallStack) => TestReq -> Either HTTP.Status BSL8.ByteString -> IO ()}

checkWithMockDB :: (?loc :: CallStack) => MockDB -> Check
checkWithMockDB db = Check $ \testReq expected -> do
  let baseURL = "http://localhost:"
  testWithApplication (app db) $ \port -> do
    manager <- Client.newManager Client.defaultManagerSettings
    preReq <- Client.parseRequest $ baseURL ++ show port ++ getPath testReq
    let req = case testReq of
          GET_{..} -> preReq { Client.method = "GET"}
          POST_{..} -> preReq {
            Client.method = "POST",
            Client.requestBody = Client.RequestBodyLBS $ encode requestBody,
            Client.requestHeaders = [(hContentType, "application/json")]
          }
    res <- Client.httpLbs req manager
    case expected of
      Right expectedBody -> do
        Client.responseStatus res `shouldBe` ok200
        Client.responseBody res `shouldBe` expectedBody
      Left expectedStatus -> do
        Client.responseStatus res `shouldBe` expectedStatus
