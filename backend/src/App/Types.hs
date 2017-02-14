{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
module App.Types where

import           Data.Aeson
import           Data.Hashable
import           Data.Swagger
import           Data.Typeable
import           GHC.Generics
import           Servant
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

-- * Datatypes:

data Todo = MkTodo {
  value :: String
} deriving (Show, Generic, Typeable)

newtype TodoId = TodoId String
  deriving (Show, Generic, Hashable, Typeable, ToJSON, ToJSONKey, FromHttpApiData, Arbitrary, Ord, Eq)

-- * Instances

instance Arbitrary Todo where
  arbitrary = MkTodo <$> arbitrary

instance ToJSON Todo
instance FromJSON Todo
instance ToSchema Todo

instance ToSchema TodoId
instance ToParamSchema TodoId
