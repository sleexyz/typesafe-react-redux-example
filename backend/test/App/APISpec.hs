{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module App.APISpec where

import           Data.Aeson
import           Control.Lens ((&))
import           GHC.Stack
import           Test.Hspec
import           Network.Wai.Handler.Warp
import           Servant.Swagger
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Network.HTTP.Client as C
import qualified Network.HTTP.Types as HTTP

import           App.App
import           App.API
import           App.Model
import           App.Types

spec :: Spec
spec = do
  describe "Datatype encodings:" $ do
    context "toJSON works for"$ do
      validateEveryToJSON appApi

  describe "API:" $ do
    let dummyTodoA = MkTodo { value = "dummyTodoA" }
    let dummyTodoB = MkTodo { value = "dummyTodoB" }

    describe "GET /todos/{id}" $ do
      let mockDB = dbEmpty
            & fst . dbAddTodo dummyTodoA
      it "should return an existing entity" $ do
        checkWithMockDB mockDB $ \Check{..} -> do
          TestGET "/todos/0" ==> \res -> do
            C.responseBody res `shouldBe` encode dummyTodoA

      it "should return 404 when entity is absent" $ do
        checkWithMockDB mockDB $ \Check{..} -> do
          TestGET "/todos/1" ==> \res -> do
            C.responseStatus res `shouldBe` HTTP.status404

    describe "GET /todos" $ do
      it "should return empty object on empty DB" $ do
        let mockDB = dbEmpty
        checkWithMockDB mockDB $ \Check{..} -> do
          TestGET "/todos" ==> \res -> do
            C.responseBody res `shouldBe` "{}"

      it "should return an existing entities" $ do
        let mockDB = dbEmpty
              & fst . dbAddTodo dummyTodoA
        checkWithMockDB mockDB $ \Check{..} -> do
          TestGET "/todos" ==> \res -> do
            C.responseBody res `shouldBe` "{\"0\":{\"value\":\"dummyTodoA\"}}"

    describe "POST /todos" $ do
      it "works" $ do
        let mockDB = dbEmpty
        checkWithMockDB mockDB $ \Check{..} -> do
          TestPOST "/todos" dummyTodoA ==> \res -> do
            C.responseStatus res `shouldBe` HTTP.status200
            C.responseBody res `shouldBe` encode ("0" :: String)
          TestPOST "/todos" dummyTodoB ==> \res -> do
            C.responseStatus res `shouldBe` HTTP.status200
            C.responseBody res `shouldBe` encode ("1" :: String)
          TestGET "/todos/0" ==> \res -> do
            C.responseBody res `shouldBe` encode dummyTodoA
          TestGET "/todos/1" ==> \res -> do
            C.responseBody res `shouldBe` encode dummyTodoB

    describe "DELETE /todos/{id}" $ do
      let mockDB = dbEmpty
            & fst . dbAddTodo dummyTodoA
      it "should work" $ do
        checkWithMockDB mockDB $ \Check{..} -> do
          TestDELETE "/todos/0"  ==> \res -> do
            C.responseStatus res `shouldBe` HTTP.status200
            C.responseBody res `shouldBe` encode("0" :: String)
          TestGET "/todos/0" ==> \res -> do
            C.responseStatus res `shouldBe` HTTP.status404

    describe "UPDATE /todos/{id}" $ do
      let mockDB = dbEmpty
            & fst . dbAddTodo dummyTodoA
      it "should work" $ do
        checkWithMockDB mockDB $ \Check{..} -> do
          TestGET "/todos/0" ==> \res -> do
            C.responseBody res `shouldBe` encode dummyTodoA
          TestPUT "/todos/0" dummyTodoB  ==> \res -> do
            C.responseStatus res `shouldBe` HTTP.status200
            C.responseBody res `shouldBe` encode dummyTodoB
          TestGET "/todos/0" ==> \res -> do
            C.responseBody res `shouldBe` encode dummyTodoB

-- * Utilities

data TestReq =
    TestGET { path :: String }
  | TestDELETE { path :: String }
  | forall a. (ToJSON a) => TestPUT { path :: String, requestBody :: a}
  | forall a. (ToJSON a) => TestPOST { path :: String, requestBody :: a}

newtype Check = Check {
  (==>) :: (?loc :: CallStack)
    => TestReq
    -> (C.Response BSL8.ByteString -> IO ())
    -> IO ()
}

checkWithMockDB :: (?loc :: CallStack) => MockDB -> (Check -> IO ()) -> IO ()
checkWithMockDB db appCont = do
  let baseURL = "http://localhost:"
  testWithApplication (app db) $ \port -> do
    let check = Check $ \testReq testCont -> do
          manager <- C.newManager C.defaultManagerSettings
          preReq <- C.parseRequest $ baseURL ++ show port ++ path testReq
          let req = case testReq of
                TestGET{} -> preReq { C.method = "GET"}
                TestDELETE{} -> preReq { C.method = "DELETE"}
                TestPOST{ requestBody } -> preReq {
                  C.method = "POST",
                  C.requestBody = C.RequestBodyLBS $ encode requestBody,
                  C.requestHeaders = [(HTTP.hContentType, "application/json")]
                }
                TestPUT{ requestBody } -> preReq {
                  C.method = "PUT",
                  C.requestBody = C.RequestBodyLBS $ encode requestBody,
                  C.requestHeaders = [(HTTP.hContentType, "application/json")]
                }
          res <- C.httpLbs req manager
          testCont res
    appCont check
