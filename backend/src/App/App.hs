{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module App.App where

import           Control.Lens hiding ((.=), elements)
import           Data.Swagger
import           Servant
import           Servant.Swagger
import           Servant.Swagger.UI

import           App.API
import           App.Model

-- * Swagger + WAI app

swagger :: Swagger
swagger = toSwagger appApi
  & set (host) (Just "localhost:8000")
  & set (info . title) "App API"
  & set (info . description) (Just "App API")

combinedApi :: Proxy CombinedAPI
combinedApi = Proxy

type CombinedAPI =
       SwaggerSchemaUI "swagger-ui" "swagger.json"
  :<|> AppAPI
  :<|> Get '[JSON, PlainText] NoContent

app :: MockDB -> IO Application
app db = do
  appServer <- toAppServer <$> makeEndpoints db
  let combinedServer =
             swaggerSchemaUIServer swagger
        :<|> appServer
        :<|> throwError (err303 { errHeaders = [("Location", "/swagger-ui")] })
  return $ serve combinedApi combinedServer
